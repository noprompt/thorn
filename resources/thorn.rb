require 'sass'

module Thorn
  class DataVisitor < ::Sass::Tree::Visitors::Base
    def self.visit (root)
      new(root).send(:visit, root)
    end

    def initialize (node)
      @node = node
    end 

    def tag(name, *content)
      {:tag => name, :content => content}
    end

    def visit (node)
      case node
      when ::Sass::Script::Node
        m = "visit_" << node.class.to_s.downcase.split("::")[1..-1].join("_")
        begin
          send(m, node)
        rescue
          puts "#{m} not implemented for #{node.class.to_s}"
        end
      when ::Sass::Supports::Negation
        visit_supports_negation(node)
      when ::Sass::Supports::Declaration
        visit_supports_declaration(node)
      when ::Sass::Supports::Operator
        visit_supports_operator(node)
      when ::String
        node
      when ::Array
        node.map {|x| visit(x)}
      when ::Hash
        node.to_a # Not sure if this is the right idea.
      when ::NilClass
        nil
      else
        super
      end
    end

    ### Script "nodes"

    def visit_script_bool (node)
      tag(:bool, node.value)
    end

    def visit_script_color (node)
      tag(:color, node.inspect)
    end

    # TODO: This only looks at `x.args` but should probably also look
    # at `x.keywords` and `x.splat`.
    def visit_script_funcall (node)
      tag(:call, tag(:var, node.name), *node.args.map {|x| visit(x)})
    end

    def visit_script_interpolation (node)
      visit(node.instance_variable_get("@mid"))
    end

    def visit_script_list (node)
      m = {
        :space => :space_separated_list,
        :comma => :comma_separated_list
      }

      tag(m[node.separator], *node.children.map {|x| visit(x)})
    end

    def visit_script_number (node)
      if node.numerator_units.empty? && node.denominator_units.empty?
        tag(:number, node.value)
      else
        number = tag(:number, node.value)
        numerator =
          unless node.numerator_units.empty?
            tag(:numerator_units, *node.numerator_units)
          end
        denominator =
          unless node.denominator_units.empty?
            tag(:denominator_units, *node.denominator_units)
          end

        tag(:unit, number, numerator, denominator)
      end
    end

    def visit_script_null (node)
      tag(:null, nil)
    end

    def visit_script_operation (node)
      operator = tag(:operator, node.operator)
      lhs = visit(node.operand1)
      rhs = visit(node.operand2)

      tag(:operation, operator, lhs, rhs)
    end

    def visit_script_unaryoperation (node)
      operator = tag(:operator, node.operator)
      tag(:unary_operation, operator , visit(node.operand))
    end

    def visit_script_variable (node)
      tag(:var, node.name)
    end

    def visit_script_string (node)
      tag(:string, node.to_s)
    end

    def visit_root (node)
      node.children.map {|x| visit(x)}
    end

    def visit_charset (node)
      tag(:charset, node.name)
    end

    def visit_comment (node)
      tag(:comment, *node.value)
    end

    def visit_each (node)
      tag(:each, visit(node.var), visit(node.list), visit(node.children))
    end

    def visit_extend (node)
      tag(:extend, node.selector.map {|x| visit(x)})
    end

    def visit_for (node)
      from = tag(:from, visit(node.from))
      to_or_through = tag((node.exclusive ? :to : :through), visit(node.to))
      children = visit(node.children)

      tag(:for, from, to_or_through, children)
    end

    # TODO: This only looks at `node.args` but should probably also look
    # at `node.splat`.
    def visit_function (node)
      var = tag(:var, node.name)
      argslist = tag(:argslist, node.args.map {|x| visit(x)})
      children = node.children.map {|x| visit(x)}

      tag(:function, var, argslist, *children)
    end

    def visit_if (node)
      # If `node.expr` is empty this is an else branch.
      tag(:if, visit(node.expr), visit(node.children), visit(node.else))
    end

    def visit_import (node)
      tag(:import, node.imported_filename)
    end

    def visit_media (node)
      tag(:media, visit(node.query), *visit(node.children))
    end

    # @include
    def visit_mixin (node)
      keywords =
        unless node.keywords.empty?
          tag(:kwargs, visit(node.keywords))
        end

      # I have no clue what this is for.
      splat = node.splat && tag(:splat, visit(node.splat))
      name = tag(:var, node.name)
      argslist = tag(:argslist, *node.args.map {|x| visit(x)}.push(keywords))

      tag(:mixin, name , argslist)
    end

    # @mixin
    def visit_mixindef (node)
      splat = node.splat && tag(:splat, visit(node.splat))
      args = node.args.map {|arg| visit(arg)}
      argslist = tag(:argslist, *args.push(splat))
      children = node.children.map {|arg| visit(arg)}

      tag(:mixin_def, argslist, children)
    end

    # @return
    def visit_return (node)
      tag(:return, visit(node.expr))
    end

    def visit_prop (node)
      property = tag(:property, *node.name)
      value = tag(:value, visit(node.value))
      tag(:declaration, property, value)
    end 
    alias :visit_supports_declaration :visit_prop

    def visit_rule (node)
      selector = tag(:selector, *node.rule.map {|x| visit(x)})
      children = node.children.map {|x| visit(x)}
      tag(:rule, selector, *children)
    end

    ### Begin @supports

    def visit_supports (node)
      tag(:supports, visit(node.condition), *visit(node.children))
    end

    def visit_supports_negation (node)
      operator = tag(:operator, :not)
      tag(:operation, operator, visit(node.condition))
    end

    def visit_supports_operator (node)
      operator = tag(:operator, node.op.to_sym)
      lhs = visit(node.left)
      rhs = visit(node.left)
      tag(:operation, operator, lhs, rhs)
    end

    ### End @supports

    def visit_variable (node)
      tag(:assign, tag(:var, node.name), visit(node.expr))
    end
  end

  def self.scss_to_data (filepath)
    filename = ::File.basename(filepath, ::File.extname(filepath)) 
    contents = ::File.read(filepath)

    root = ::Sass::SCSS::Parser.new(contents, filepath).parse
    ::Sass::Tree::Visitors::SetOptions.visit(root, {:style => :compressed})
    ::Sass::Tree::Visitors::CheckNesting.visit(root)

    DataVisitor.visit(root)
  end
end

Thorn.scss_to_data("/Users/noprompt/git/jlong/sass-bootstrap/lib/_mixins.scss")
