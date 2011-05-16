(ns jaco.crud.core
  (:use jaco.crud.routes
        [compojure.core     :only [defroutes]]
        (jaco.core [routes  :only [route]]
                   [actions :only [*error-handler*]]))
  (:require (jaco.crud [actions   :as actions]
                       [templates :as templates])))

(defroutes main-routes
  (route #'index    actions/index)
  (route #'overview actions/overview)
  (route #'create   actions/create-page)
  (route #'update   actions/update-page)

  (route #'create :post actions/create)
  (route #'update :post actions/update)
  (route #'delete :get  actions/delete))

(def ^{:macro true} defcrud @#'actions/defcrud)
