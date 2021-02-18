(ns polylith.clj.core.workspace-clj.definitions-test
  (:require [clojure.test :refer :all]
            [polylith.clj.core.workspace-clj.definitions :as defs]))

(deftest filter-statements--returns-def-statements
  (let [code '((ns polylith.spec.interface
                 (:require [polylith.spec.core :as core]))
               (defn valid-config? ['config]
                 (core/valid-config? 'config)))]
    (is (= '((defn valid-config? ['config]
               (core/valid-config? 'config)))
           (defs/filter-statements code)))))

(deftest statement-attributes--def-statement--returns-empty-map
  (is (= {}
         (defs/statement-attributes defs/data-type '(def my-data 1)))))

(deftest statement-attributes--defn-statement--returns-correct-attr
  (are [expected-attr statement] (= expected-attr (defs/statement-attributes defs/function-type statement))
    {}                  '(defn my-func [])
    {:doc "my doc"}     '(defn my-func "my doc" [])
    {:version 1}        '(defn my-func {:version 1} [])
    {:version 1
     :doc     "my doc"} '(defn my-func "my doc" {:version 1} [])))

(deftest statement-attributes--defmacro-statement--returns-correct-attr
  (are [expected-attr statement] (= expected-attr (defs/statement-attributes defs/macro-type statement))
    {}                  '(defmacro my-macro [])
    {:doc "my doc"}     '(defmacro my-macro "my doc" [])
    {:version 1}        '(defmacro my-macro {:version 1} [])
    {:version 1
     :doc     "my doc"} '(defmacro my-macro "my doc" {:version 1} [])))

(deftest definitions--a-single-arity-defn-statement--returns-a-definition
  (is (= [{:name "ordered-map"
           :type defs/function-type
           :attr {}
           :parameters [{:name "&"}
                        {:name "keyvals"}]}]
         (defs/definitions "interface"
                           '(defn ordered-map [& keyvals] (core/ordered-map keyvals))
                           "interface"))))

(deftest definitions--a-single-arity-defn-statement-with-a-type-hint--returns-a-definition-including-type-hint
  (is (= [{:name "my-func"
           :type defs/function-type
           :attr {}
           :parameters [{:name "arg1"}
                        {:name "arg2", :type "^String"}]}]
         (defs/definitions "interface"
                           '(defn my-func [arg1 ^String arg2] (core/my-func arg1 arg2))
                           "interface"))))

(deftest definitions--a-multi-arity-defn-statement--returns-a-list-of-definitions
  (is (= [{:name "pretty-messages"
           :type defs/function-type
           :attr {}
           :parameters [{:name "workspace"}]}
          {:name "pretty-messages"
           :type defs/function-type
           :attr {}
           :parameters [{:name "messages"}
                        {:name "color-mode"}]}]

         (defs/definitions "interface"
                           '(defn pretty-messages
                              ([workspace]
                               (msg/pretty-messages workspace))
                              ([messages color-mode]
                               (msg/pretty-messages messages color-mode)))
                           "interface"))))

(deftest definitions--returns-provided-attr
  (= [{:name "my-func"
       :type defs/function-type
       :attr {:doc "my doc" :version 1}
       :parameters []}]
     (defs/definitions "interface"
       '(defn my-func "my doc" {:version 1} [])
       "interface")))
