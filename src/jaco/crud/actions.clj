(ns jaco.crud.actions
  (:refer-clojure :exclude [type])
  (:use [jaco.core.actions :only [*request* *errors* defaction action converter]]
        [clojure.contrib.macro-utils :only [macrolet]])
  (:require (jaco.crud [datasource :as ds]
                       [templates  :as tpl]
                       [routes     :as routes])))

(def *crud-map* {})

(defn props [type]
  (or (get *crud-map* (-> type .getName keyword))
      (throw (IllegalArgumentException.
              (str "There is no CRUD for the type: " (.getName type))))))

(def type [:* (converter #(Class/forName %)
                         (constantly "There is no class with the given name."))])

(def id [:id (converter #(and (seq %) (Integer/parseInt %))
                        (constantly "Wrong entity ID."))])


(macrolet [(with-default [sym & body]
             `(if-let [~sym (~(keyword sym) (props ~(symbol "type")))]
                (~sym (:params *request*))
                (do ~@body)))]

  (defaction create [type]
    (with-default create
      (ds/save! ((:factory (props type)) (:params *request*)))
      (tpl/completed)))

  (defaction update [type id]
    (with-default update
      (when-let [entity (ds/retrieve type id)]
        (ds/save! (merge entity
                         (select-keys (:params *request*) (keys entity))))
        (tpl/completed))))

  (defaction delete [type id]
    (with-default delete
      (ds/delete! (ds/retrieve type id))
      (tpl/completed))))


(defn- apply-to-string [type m]
  (let [fields (-> type props :fields)]
    (into {} (for [[k v] (select-keys m (keys fields))]
               [k ((-> fields k :to-string) v)]))))

;; :view - fn of 2 args, elem name and its value
(defmacro replace-view [type expr]
  (let [k 'k, m 'm]
    `(map (fn [[~k ~m]]
            (-> ~m
                (assoc :view ((:view ~m) (name ~k) ~expr))
                (dissoc :default :to-string)))
          (:fields (props ~type)))))


;; :default - fn of no args, which used to get default value when creating new entity
(defaction create-page [type]
  (tpl/view nil (replace-view type ((:default m)))))

(defaction update-page [type id]
  (when-let [entity (ds/retrieve type id)]
    (tpl/view id (replace-view type (k (apply-to-string type entity))))))

(defaction overview [type]
  (let [name (.getName type), keys (:overview (props type))]
    (tpl/overview (for [e (ds/retrieve-all type)]
                    [(routes/update name (ds/get-id e))
                     (routes/delete name (ds/get-id e))
                     (apply-to-string type (select-keys e keys))])
                  (routes/create name))))

(defaction index []
  (tpl/index (map (fn [[k v]]
                    {:title (:title v)
                     :url (routes/overview (.getName k))})
                  *crud-map*)))


;;================================
;; The Macro
;;================================

(defrecord Field [title comment view default to-string])

(defn provide-default-opts [type fields]
  {:title (str type)
   :overview (map first fields)
   :factory (fn [_]
              (throw (UnsupportedOperationException. "Factory fn is not provided")))})

(defn alter-crud-map
  [type opts fields]
  (letfn [(make-field [[name title & {:keys [comment view default to-string]}]]
            [name (Field. title comment
                          (or view tpl/default-view)
                          (or default str)
                          (or to-string str))])]
    `(alter-var-root (var *crud-map*) assoc (-> ~type .getName keyword)
                     ~(assoc opts :fields (into {} (map make-field fields))))))

(defmacro defcrud
  {:arglists '([type opts? & fields])}
  [type & opts+fields]
  (let [[opts fields] (if (map? (first opts+fields))
                        [(first opts+fields) (rest opts+fields)]
                        [nil opts+fields])
        opts (merge (provide-default-opts type fields) opts)]
    (alter-crud-map type opts fields)))
