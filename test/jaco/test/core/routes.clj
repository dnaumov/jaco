(ns jaco.test.core.routes
  (:use midje.sweet)
  (:use jaco.core.routes :reload-all)
  (:import jaco.core.routes.Route))

(defn request [handler uri & options]
  (handler (conj {:request-method :get :uri uri} (apply hash-map options))))

(defn request-route [r uri]
  (request (route r identity) uri))


(fact "you can define named route with defroute"
  (defroute simple "/foo/bar")
  (request-route simple "/foo/bar") => {}
  (request-route simple "/qux") => nil)

(fact "uri string uses the same syntax as compojure routes"
  (defroute params "/:first/:second")
  (request-route params "/foo/1") => {:first "foo" :second "1"})

(fact "you can generate handler from the named route with the route fn"
  (let [handler (route simple (constantly :ok))]
    (request handler "/foo/bar") => :ok))

(fact "then, you can combine several handlers into one by using defroutes (as in compojure)"
  (defroutes compound
    (route simple (constantly :simple))
    (route params (constantly :params)))
  (request compound "/foo/bar/baz") => :simple
  (request compound "/qwe/asd") => :params)

(fact "last arg is a fn that will be applied to params map (usually it's an action)"
  (request (route params identity) "/a/b") => {:first "a" :second "b"})

(fact "you can specify method which route will match; default one is :get"
  (let [r (route simple :post (constantly :matches))]
    (request r "/foo/bar") => nil
    (request r "/foo/bar" :request-method :post) => :matches))

(fact "you can specify regexes for uri parameters"
  (defroute regex "/:id" {:id #"[0-9]+"})
  (request-route regex "/foo") => nil
  (request-route regex "/42") => {:id "42"})

(fact "defroute also creates a fn for url generation with the same name"
  (simple) => "/foo/bar"
  (params "bar" "baz") => "/bar/baz"
  (regex "42") => "/42"
  (regex "foo") => (throws IllegalArgumentException))

(fact "you should use context macro for the right generation of relative urls"
  (defroutes your-app
    (context "/module-name"
             (route simple identity)))
  (simple) => "/module-name/foo/bar")


(facts "about url constructing"
  (defroute foo "/:a/:b")

  (foo "whoa!" "bo o?") => "/whoa%21/bo+o%3F"
  (foo "a" "b" :c "foo") => "/a/b?c=foo"
  (foo "a" nil :foo "bar")  => "/a/?foo=bar"
  (foo "a" "b" :c) => (throws RuntimeException)

  (set-context-path! "/foo" foo)
  (foo "a" "b") => "/foo/a/b")
