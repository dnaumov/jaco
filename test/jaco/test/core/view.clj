(ns jaco.test.core.view
  (:use midje.sweet)
  (:use jaco.core.view
        [jaco.core.routes :only [defview*]] :reload))

(facts "about defview's args"
  (binding [defview* (fn [& args] (butlast args))]
    (defview str :get :html) => [#'str :get :html]
    (defview str :get)       => [#'str :get :any]
    (defview str :html)      => [#'str :any :html]
    (defview :get)             => [nil :get :any]
    (defview :html)            => [nil :any :html]
    (defview :get :html)       => [nil :get :html]))


