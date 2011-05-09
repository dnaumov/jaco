(ns jaco.crud.datasource)

(def *datasource* nil)

(defn- dispatch [& _] *datasource*)

(defmulti save! dispatch)
(defmulti delete! dispatch)
(defmulti retrieve dispatch)
(defmulti retrieve-all dispatch)
(defmulti get-id dispatch)

(defprotocol EntityHooks
  (before-save [this])
  (after-load  [this]))

(extend-protocol EntityHooks
  Object
  (before-save [this] this)
  (after-load  [this] this)
  nil
  (before-save [this] nil)
  (after-load  [this] nil))


(defmacro init [datasource]
  (let [ns (symbol (str "jaco.crud.datasource." (name datasource)))]
    `(do
       (alter-var-root (var *datasource*) (constantly ~datasource))
       (require '~ns))))
