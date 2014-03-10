(ns thorn.core
  (:require [zweikopf.core :as zweikopf]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(zweikopf/init-ruby-context)

(defn require-file [path]
  (-> (io/resource path)
      io/file 
      zweikopf/ruby-require))

(require-file "gems/sass-3.2.14/lib/sass")
(zweikopf/ruby-load "thorn.rb")

(defn scss->edn [file-path]
  (zweikopf/clojurize (zweikopf/call-ruby "Thorn" :scss_to_data file-path)))

;;----------------------------------------------------------------------
;; Utilities

;; Tag helpers

(defn remove-tag
  "Recursively remove nodes which satisfy pred."
  ([pred]
     (fn [data]
       (remove-tag pred data)))
  ([pred data]
     (into (empty data)
           (mapcat
            (fn [tag]
              (when-not (pred tag)
                (list (if (map? tag)
                        (update-in tag [:content] #(remove-tag pred %))
                        tag))))
            data))))

(defn filter-tag
  "Recursively find tags which satisfy pred."
  ([pred]
     (fn [data]
       (filter-tag pred data)))
  ([pred data]
     (mapcat
      (fn [tag]
        (if (pred tag)
          (concat (list tag) (filter-tag pred (:content tag)))
          (filter-tag pred (:content tag))))
      data)))

(defn tag=
  ([tag-name]
     (fn [tag] (tag= tag tag-name)))
  ([tag tag-name]
     (= (:tag tag) tag-name)))

(defn tag? [x]
  (and (map? x)
       (contains? x :tag)
       (contains? x :content)))

(def comment? (tag= :comment))
(def space-separated-list? (tag= :space_separated_list))
(def comma-separated-list? (tag= :comma_separated_list))
(def unit? (tag= :unit))
(def operator? (tag= :operator))
(def call? (tag= :call))
(def declaration? (tag= :declaration))

(def remove-comments (remove-tag comment?))
(def remove-declarations (remove-tag declaration?))
(def filter-declarations (filter-tag declaration?))
(def filter-calls (filter-tag call?))

(defn property->string [{:keys [content]}]
  (first content))

(defn operator->string [{:keys [content]}]
  (let [m {:single_eq "="}
        op (first content)]
    (or (m op)
        ;; This should only happen while m hasn't accounted for
        ;; everything.
        (throw (Exception. "Unknown operator type: " (pr-str op))))))

(defn unit->string [{:keys [content]}]
  (let [[number numerator-units denominator-units] content
        n (-> number :content first)
        nunits (-> numerator-units :content string/join)
        dunits (-> denominator-units :content string/join)]
    (if (ratio? n)
      (str (numerator n) nunits "/" (denominator n) dunits)
      (str n nunits (when-not (string/blank? dunits)
                      (str "/" dunits))))))

(defn var->symbol [tag]
  (-> tag :content first symbol))

(defn safe-comma-split
  "Like (string/split s #\",\") but accounts for string literals."
  [s]
  (let [re #"(?:\"(?:\\|[^\"])*\"|'(?:\\|[^'])*'|[^,])+"]
    (re-seq re s)))

(defn selector-split [s]
  (map string/trim (safe-comma-split s)))

;;----------------------------------------------------------------------
;; String Emitter

(defmulti tag->str :tag)

(defmethod tag->str :string [{:keys [content]}]
  (first content))

(defmethod tag->str :unit [{:keys [content]}]
  (let [[number numerator-units denominator-units] content
        n (-> number :content first)
        nunits (-> numerator-units :content string/join)
        dunits (-> denominator-units :content string/join)]
    (if (ratio? n)
      (str (numerator n) nunits "/" (denominator n) dunits)
      (str n nunits (when-not (string/blank? dunits)
                      (str "/" dunits))))))


;;----------------------------------------------------------------------
;; CSS Emitter

(defmulti tag->clj :tag)

(defmethod tag->clj :comment [{:keys [content]}]
  (cons 'comment (mapcat #(string/split % #"\n") content)))

(defmethod tag->clj :string [{:keys [content]}]
  (first content))

(defmethod tag->clj :number [{:keys [content]}]
  (first content))

(defmethod tag->clj :space_separated_list [{:keys [content]}]
  (vec (map tag->clj content)))

(defmethod tag->clj :comma_separated_list [{:keys [content]}]
  (vec (map tag->clj content)))

(defmethod tag->clj :unit [{:keys [content]}]
  (let [[number numerator-units _] content
          nunits (-> numerator-units :content)
          n (tag->clj number)]
      (case (count nunits)
        0 n
        1 (list (symbol "garden.units" (first nunits)) n))))

(defmethod tag->clj :operation [{:keys [content]}]
  (let [[operator lhs rhs] content]
    (str (tag->clj lhs)
         (operator->string operator)
         (tag->clj rhs))))

;; Rules

(defmethod tag->clj :rule [{:keys [content]}]
  (let [[selector & children] content]
    (vec (concat (tag->clj selector) (map tag->clj children)))))

(defmethod tag->clj :selector [{:keys [content]}]
  (selector-split (first content)))

(defmethod tag->clj :declaration [{:keys [content]}]
  (let [[property value] (map tag->clj content)]
    ;; TODO: Parse the value when possible.
    {property value}))

(defmethod tag->clj :property [{:keys [content]}]
  (-> content first keyword))

(defmethod tag->clj :value [{:keys [content]}]
  (let [x (first content)
        v (tag->clj (first content))]
    (if (space-separated-list? x)
      [v]
      v)))

;; Call

(defmethod tag->clj :call [{:keys [content]}]
  (let [[var & args] content]
    (cons (tag->clj var) (map tag->clj args))))

(defmethod tag->clj :var [{:keys [content]}]
  (-> content first symbol))

(defmethod tag->clj :media [{:keys [content]}]
  (let [[query & children] content
        query-str (reduce
                   (fn [s x]
                     (str s (if (tag? x)
                              (tag->str x)
                              x)))
                   ""
                   query)]
    `(garden.stylesheet/at-media ~query-str
       ~@(map tag->clj children))))

(defmethod tag->clj nil [_]
  nil)

;;----------------------------------------------------------------------
;; Namespace generation

(defn distinct-calls [data]
  (distinct
   (map
    (fn [{:keys [content]}]
      (-> content first var->symbol))
    (filter-calls data))))

(defn call-defs [data]
  (map
   (fn [sym]
     `(garden.def/defcssfn ~sym))
   (distinct-calls data)))

(defn ns-spec [ns-name]
  `(~'ns ~ns-name
     (:require ~'[garden.stylesheet]
               ~'[garden.units]
               ~'[garden.def])))

(defn scss->clj [filename namespace-name]
  (let [spec (ns-spec (symbol namespace-name))
        data (remove-comments (scss->edn filename))
        defs (call-defs data)
        styles (map tag->clj data)
        styles-name (-> (name namespace-name)
                        (string/split  #"\.")
                        last
                        symbol)
        styles-spec (concat `(garden.def/defstyles ~styles-name) styles)]
    `(~spec
      ~@defs
      ~styles-spec)))
