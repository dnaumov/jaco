(ns jaco.crud.core
  (:use jaco.crud.routes
        [jaco.core.routes :only [defroutes route]])
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
