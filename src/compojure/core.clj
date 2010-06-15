(ns compojure.core
  "A concise syntax for generating Ring handlers."
  (:use [ring.middleware params cookies]
        clout.core
        compojure.response))

(defn- method-matches
  "True if this request matches the supplied method."
  [method request]
  (let [request-method (request :request-method)
        form-method    (get-in request [:form-params "_method"])]
    (or (nil? method)
        (if (and form-method (= request-method :post))
          (= (.toUpperCase (name method)) form-method)
          (= method request-method)))))

(defn- prepare-route
  "Pre-compile the route."
  [route]
  (cond
    (string? route)
      (route-compile route)
    (vector? route)
      (route-compile
        (first route)
        (apply hash-map (rest route)))
    :else
      `(if (string? ~route)
         (route-compile ~route)
         ~route)))

(defn- merge-route-params
  "Associate route parameters with the request map."
  [request params]
  (merge-with merge request {:route-params params, :params params}))

(defn- param-vector-bindings
  "Create the bindings for a vector of parameters."
  [request bindings body]
  (let [[args [_ more]] (split-with #(not= % '&) bindings)]
    `(let [{:strs ~(vec args)} (~request :params)
          ~@(if more [more `(dissoc (~request :params) ~@(map keyword args))])]
       ~@body)))

(defmacro compile-route [method route bindings body]
  (let [route (prepare-route route)
        route-fn `(fn [~bindings] ~@body)]
    `[~method ~route (quote ~bindings) ~route-fn]))

(defn combine-routes
  ""
  [& routes]
  (apply concat routes))

(defn combine-handlers [ & handlers]
  (fn [request]
    (some #(% request) handlers)))

(defn- apply-doc
  "Return a symbol and body with an optional docstring applied."
  [name doc? body]
  (if (string? doc?)
    (list* (vary-meta name assoc :doc doc?) body)
    (list* name doc? body)))

(defmacro defroutes
  "Define a Ring handler function from a sequence of routes. Takes an optional
  doc-string."
  [name doc? & routes]
  (let [[name & routes] (apply-doc name doc? routes)]
   `(def ~name
      (combine-routes ~@routes))))

(defn get-bindings [bindings request]
  (if (vector? bindings)
    (map #(or (get (request :params) (str %))
              (get (request :params) (keyword %))) bindings)
    nil))

(defn- matches? [request [method path bindings route-fn]]
  (and (method-matches method request) (route-matches path request)))

(defn find-matching-route
  "middleware that finds the matching route, if any, and adds it to the request"
  [handler routes]
  (fn [request]
    (if-let [[method path bindings route-fn] (first (filter #(matches? request %) routes))]
      (let [route-params (route-matches path request)
            request (-> request
                        (assoc :route-fn route-fn)
                        (assoc :route-params route-params)
                        (merge-route-params route-params)
                        (assoc :route-bindings bindings))]
        (handler request))
      (handler request))))

(defn call-matching-route
  "If the request has a matching route, calls it and returns the response, else continues calling the middleware chain"
  [handler]
  (fn [request]
    (if (:route-fn request)
      (render request
              (if (:route-bindings request)
                (apply (:route-fn request) (get-bindings (:route-bindings request) request))
                ((:route-fn request) request)))
      (handler request))))
