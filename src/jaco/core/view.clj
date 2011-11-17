(ns jaco.core.view
  (:use [jaco.core.routes :only [defview*]]))

(defmacro defview
  "TODO: write"
  {:arglists '([route? method? output-format?])}
  [& [maybe-route & more :as all]]
  (let [method? #{:get :post :put :delete :head}
        [route [maybe-method & [maybe-output & more :as all]]]
        (if (symbol? maybe-route)
          [maybe-route more]
          [nil all]),
        [method & [maybe-output more :as all]]
        (if (method? maybe-method)
          [maybe-method maybe-output more]
          [:any maybe-method all]),
        [output [args & body]]
        (if (keyword? maybe-output)
          [maybe-output more]
          [:any (cons maybe-output more)])]
    `(defview* ~(when route `(var ~route)) ~method ~output (fn [~@args] ~@body))))








