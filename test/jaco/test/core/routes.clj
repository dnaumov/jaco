(ns jaco.test.core.routes
  (:use midje.sweet
        [jaco.core.actions :only [*request*]]
        [compojure.core    :only [defroutes]])
  (:use jaco.core.routes :reload-all))

(defn request [handler uri & options]
  (:body (handler (conj {:request-method :get :uri uri} (apply hash-map options)))))

(defn request-route [r uri]
  (request (route r #(hash-map :body %)) uri))


(fact "you can define named route with defroute"
  (defroute simple "/foo/bar")
  (request-route #'simple "/foo/bar") => {}
  (request-route #'simple "/qux") => nil)

(fact "uri string uses the same syntax as compojure routes"
  (defroute params "/:first/:second")
  (request-route #'params "/foo/1") => {:first "foo" :second "1"})

(fact "you can generate handler from the named route with the route fn"
  (let [handler (route #'simple (constantly "ok"))]
    (request handler "/foo/bar") => "ok"))

(fact "then, you can combine several handlers into one by using defroutes (as in compojure)"
  (defroutes compound
    (route #'simple (constantly "simple"))
    (route #'params (constantly "params")))
  (request compound "/foo/bar") => "simple"
  (request compound "/qwe/asd") => "params")

(fact "last arg is a fn that will be applied to params map (usually it's an action)"
  (request (route #'params (fn [{:keys [first second]}] (str first second)))
           "/foo/bar") => "foobar")

(fact "you can specify method which route will match; default one is :get"
  (let [r (route #'simple :post (constantly "matches"))]
    (request r "/foo/bar") => nil
    (request r "/foo/bar" :request-method :post) => "matches"))

(fact "you can specify regexes for uri parameters"
  (defroute regex "/:id" {:id #"[0-9]+"})
  (request-route #'regex "/foo") => nil
  (request-route #'regex "/42") => {:id "42"})

(fact "route binds *request* to the current request map"
  (request (route #'simple (fn [_] {:body *request*})) "/foo/bar")
  => (contains {:request-method :get, :uri "/foo/bar", :params {}}))

(fact "defroute also creates a fn for url generation"
  (simple) => "/foo/bar"
  (params "bar" "baz") => "/bar/baz"
  (regex "42") => "/42"
  (regex "foo") => (throws IllegalArgumentException))

(fact "you should use context macro for the correct generation of relative urls"
  (defroutes your-app
    (context "/module-name"
             (route #'simple identity)))
  (simple) => "/module-name/foo/bar")


(facts "about url constructing"
  (defroute foo "/:a/:b")

  (foo "whoa!" "bo o?") => "/whoa%21/bo+o%3F"
  (foo "a" "b" :c "foo") => "/a/b?c=foo"
  (foo "a" nil :foo "bar")  => "/a/?foo=bar"
  (foo "a" "b" :c) => (throws IllegalArgumentException)

  (set-context-path! #'foo "/foo")
  (foo "a" "b") => "/foo/a/b")


(facts "about make-route"
  (let [r (make-route :get "/foo" nil [(constantly "foo!")])]
    (request r "/foo") => "foo!"
    (request r "/fu")  => nil)

  (let [r (make-route :get "/foo/:id" {:id #"[0-9]+"} [(constantly "foo!")])]
    (request r "/foo/brr") => nil
    (request r "/foo/42") => "foo!")

  (request (make-route :get "/foo/:bar" nil [:bar #(.toUpperCase %)]) "/foo/baar")
  => "BAAR")


(facts "about set-context-path!"
  (letfn [(ctxt [v] (:jaco.core.routes/context-path (meta v)))]
    (set-context-path! #'simple "/ctxt")
    (set-context-path! #'params "/foo")
    (ctxt #'simple) => "/ctxt"
    (ctxt #'params) => "/foo"))
