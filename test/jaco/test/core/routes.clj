(ns jaco.test.core.routes
  (:use midje.sweet)
  (:use jaco.core.routes
        [jaco.core.actions :only [defaction]]
        [jaco.core.view    :only [defview]] :reload)
  (:import jaco.core.routes.NamedRoute))

;; Some helper functions

(defn request [h url & params]
  (let [f (fn [default] #(if (seq %) % default))
        with-default #(-> %
                          (update-in [:actions] (f {:get identity}))
                          (update-in [:views] (f {:any {:any (fn [x] {:body x})}})))
        h (if (named-route? h) (handler [(with-default h)]) h)]
    (:body (h (merge {:request-method :get :uri url}
                     (apply hash-map params))))))

(def matches? (comp boolean request))


(fact "Named route is defined with the defroute macro."
  (defroute foo "/foo")
  (matches? foo "/foo") => true
  (matches? foo "/qux") => false
  foo => (just (NamedRoute. {} {} "/foo" nil fn?)))

(fact "URL string uses the same syntax as Compojure's one."
  (defroute params "/:first/:second")
  (request params "/foo/1") => {:first "foo" :second "1"})

(fact "You can specify regexes for the params."
  (defroute regex "/:id" {:id #"[0-9]+"})
  (:opts regex) =>  (just {:id anything})
  (request regex "/foo") => nil
  (request regex "/42") => {:id "42"})

(fact "defroute also creates a fn which will generate corresponding URL."
  (foo) => "/foo"
  (params "bar" "baz") => "/bar/baz"
  (regex "42") => "/42"
  (regex "foo") => (throws IllegalArgumentException))

(fact "It's possible to specify URL parameters and anchor."
  (foo :baz "qux") => "/foo?baz=qux"
  (foo :# "anchor") => "/foo#anchor")

(fact "Then, define action for the route. Application logic goes here."
  (defaction foo :get [] "action")
  (defaction params :get [first second] (str first second))
  (request foo "/foo") => "action"
  (request params "/a/b") => "ab")

(fact "If you set only route, the action will match any method."
  (defaction foo [] "match")
  (request foo "/foo" :request-method :put) => "match")

(fact "If action or view returns nil, then the whole route returns
       nil and so doesn't match the request."
  (defaction regex :get [_] nil)
  (request regex "/1") => nil)

(fact "Var *request* will be bound to the current ring request map."
  (defaction foo :get [_] *request*)
  (request foo "/foo") => (contains {:request-method :get, :uri "/foo", :params {}}))

(fact "Value returned by action will be passed to the view function. Last arg
       specifies the value of the *output* var at which the view will be applied."
  (defview params :get :html [x]   
    (str "<html>" x "</html>"))
  (with-output :html
    (request params "/foo/bar")) => "<html>foobar</html>")

(fact "You don't have to write the last arg if you don't plan to make
       different output formats - view will match any *output* value."
  (defview foo :get [_] "match")
  (request foo "/foo") => "match")

(fact "Also it's possible to define generic views which will be applied to
       all routes. In such case one should specify only output format."
  (defview :xml [_] "xmlized")
  (with-output :xml
    (request foo "/foo" :request-method :post) => "xmlized"
    (request params "/a/b") => "xmlized"))

(fact "In fact, you can omit any arg and make whatever combination you want."
  (defview foo [_] "route")
  (defview :get [_] "method")
  (defview foo :html [_] "route and output")
  (with-output :whatever
    (request foo "/foo" :request-method :put) => "route"
    (request params "/1/2") => "method")
  (with-output :html
    (request foo "/foo" :request-method :delete) => "route and output"))

(fact "You can combine several routes into one handler using defhandler macro."
  (defhandler combined
    #'foo #'params)
  (matches? combined "/foo") => true
  (matches? combined "/a/b") => true)

;; TODO:
(future-fact "it's possible to specify error handler for the routes"
             (defroutes safe {:error-handler (constantly "error")}
               (route #'params #(if (= "good" (:first %)) "ok" (*error-handler*))))
             (request safe "/good/second") => "ok"
             (request safe "/bad/second") => "error")

(fact "As well as coll of middleware functions."
  (let [replacer (fn [h] (fn [r] {:body "fixed"}))]
    (defhandler modified {:middleware [replacer]}
      #'foo)
    (request modified "/foo") => "fixed"))

(fact "You should use context macro in order to generate relative urls correctly."
  (defhandler module-routes
    #'foo)
  (defhandler your-app
    (context "/module-name" module-routes))
  (matches? your-app "/module-name/foo") => true
  (foo) => "/module-name/foo")

(fact "Contexts can be nested."
  (defhandler contextual #'params)
  (context "/there"
    (context "/will"
      (context "/be"
        (context "/a"
          (context "/long"
            contextual)))))
  (params "long" "url") => "/there/will/be/a/long/long/url")



(facts "about url constructing"
  (defroute qux "/:a/:b")

  (qux "whoa!" "bo o?") => "/whoa%21/bo+o%3F"
  (qux "a" "b" :c "qux") => "/a/b?c=qux"
  (qux "a" nil :qux "bar")  => "/a/?qux=bar"
  (qux "a" "b" :c) => (throws IllegalArgumentException)

  (set-context #'qux "/qux")
  (qux "a" "b") => "/qux/a/b")

(facts "about combine-fns"
  ((combine-fns [inc str]) 1) => "2")

(def ^{:macro true} make-route @#'jaco.core.routes/make-route)
(facts "about make-route"
  (let [r (make-route :get "/foo" nil [(constantly "foo!")])
        rr (make-route :get "/foo/:id" {:id #"[0-9]+"} [(constantly "foo!")])]

    (request r "/foo") => "foo!"
    (request r "/fu")  => nil

    (request rr "/foo/brr") => nil
    (request rr "/foo/42") => "foo!"

    (request (make-route :get "/foo/:bar" nil [:bar #(.toUpperCase %)]) "/foo/baar")
    => "BAAR"

    (request (make-route :get "/foo" nil [(constantly nil) str]) "/foo")
    => nil))


(facts "about set-context"
  (set-context #'foo "/ctxt")
  (set-context #'params "/foo")
  (:context foo) => "/ctxt"
  (:context params) => "/foo")

(comment ;; can't be done since make-route is a macro
  (facts "about construct-routes"
    (binding [make-route vector]
      (construct-routes {:actions {:get "get-action"
                                   :post "post-action"}
                         :views {:get {:any "get-view"}
                                 :any {:any "any-view"}}
                         :path "path"}))
    => [[:get "path" nil ["get-action" "get-view"]]
        [:post "path" nil ["post-action" "any-view"]]]))

(facts "about defhandler and context"
  (letfn [(routes [x] (-> x meta :jaco.core.routes/routes))]
    (defhandler a, #'foo)
    (routes a) => {#'foo nil}

    (defhandler b, a #'params)
    (routes b) => {#'foo nil, #'params nil}

    (defhandler c, (context "/foo" a) #'params)
    (routes c) => {#'foo "/foo", #'params nil}

    (routes (context "/a" (context "/b" (context "/c" a))))
    => {#'foo "/a/b/c"}

    (defhandler d, #'params)
    (routes (context "/x"
              (context "/a" a)
              (context "/d" d)))
    => {#'foo "/x/a" #'params "/x/d"}))

;; Reset contexts
(set-context #'foo "")
(set-context #'params "")
