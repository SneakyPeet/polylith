(ns polylith.clj.core.workspace-clj.definitions
  (:require [clojure.string :as str]
            [polylith.clj.core.util.interface :as util]))

(def data-type "data")
(def function-type "function")
(def macro-type "macro")

(def ->generic-type {'def data-type
                     'defn function-type
                     'defmacro macro-type})

(defn definition? [code]
  (if (list? code)
    (let [f (first code)
          private? (-> code second meta :private)]
      (and (not private?)
           (or (= f 'def)
               (= f 'defn)
               (= f 'defmacro))))
    false))

(defn filter-statements [statements]
  (filterv definition?
           ; Drops the namespace declaration on top of the file
           (drop 1 statements)))

(defn sub-namespace [namespace interface-ns]
  (when (not= namespace interface-ns)
    (str/join "." (drop 1 (str/split namespace #"\.")))))

(defn parameter [name]
  (let [type (-> name meta :tag)]
    (if type
      {:name (str name)
       :type (str "^" type)}
      {:name (str name)})))

(defn function [namespace type name code interface-ns]
  (let [sub-ns (sub-namespace namespace interface-ns)
        parameters (mapv parameter (first code))
        str-name (str name)
        str-type (str type)]
    (util/ordered-map :name str-name
                      :type str-type
                      :parameters parameters
                      :sub-ns sub-ns)))


(defn statement-attributes [type [_ _ a b]]
  (if-not (contains? #{function-type macro-type} type)
    {}
    (let [doc-string?    (string? a)
          attr-1rst-pos? (map? a)
          attr-2nd-pos?  (map? b)
          attr           (cond
                           attr-1rst-pos? a
                           attr-2nd-pos?  b
                           :else          {})]
      (cond-> attr
        doc-string? (assoc :doc a)))))


(defn definitions [namespace statement interface-ns]
  "Takes a statement (def, defn or defmacro) from source code
   and returns a vector of definitions."
  (let [type (-> statement first ->generic-type)
        name (second statement)
        attr (statement-attributes type statement)
        code (drop-while #(not (or (list? %)
                                   (vector? %)))
                         statement)
        defs (if (= data-type type)
               [(util/ordered-map :name (str name)
                                  :type (str type)
                                  :sub-ns (sub-namespace namespace interface-ns))]
               (if (-> code first vector?)
                 [(function namespace type name code interface-ns)]
                 (mapv #(function namespace type name % interface-ns) code)))]
    (map #(assoc % :attr attr) defs)))
