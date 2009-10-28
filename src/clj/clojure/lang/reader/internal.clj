(ns clojure.lang.reader.internal)
 
(defn namespace-for [sym]
  (let [ns-sym (symbol sym)]
    (or (.lookupAlias *ns* ns-sym) 
        (clojure.lang.Namespace/find ns-sym))))

(defn var->symbol [#^clojure.lang.Var v]
  (symbol (str (.ns v)) (str (.sym v))))

(defn resolve-symbol [sym]
  (let [ns   (when (.getNamespace sym)
               (namespace-for (.getNamespace sym)))
        name (str (name sym))
        res  (resolve sym)]
  (cond
    (and res (class? res)) (symbol (.getCanonicalName res))
    (and res (var? res)) (var->symbol res)  
    (not ns) (symbol (str *ns*) name)
    :else (symbol (str ns) name))))

(def *autofn-syms* nil)

(defn- get-or-set! [index themeta]
  (if-let [sym (*autofn-syms* index)]
    sym
    (let [sym (with-meta (gensym (str "p" index "__")) themeta)]
      (when (and (number? index) 
                 (> index (:max *autofn-syms* -1)))
        (set! *autofn-syms* (assoc *autofn-syms* :max index)))
      (set! *autofn-syms* (assoc *autofn-syms* index sym))
      sym)))

(defn- parse-autoarg [form]
  (cond
    (vector? form) (vec (map parse-autoarg form))
    (map? form) (into {} (map parse-autoarg form))
    (set? form) (into #{} (map parse-autoarg form))
    (seq? form) (doall (map parse-autoarg form))
    (= form '%) (get-or-set! 1 ^form)
    (= form '%&) (get-or-set! 'rest ^form)
    (= (first (str form)) \%) 
    (try (get-or-set! (Integer/parseInt (apply str (rest (str form)))) ^form) 
         (catch NumberFormatException e 
           form))
    :else form))


(defn autofn* [form]
  (let [gen-positional-params 
        (fn [max-idx] 
          (map #(*autofn-syms* % (gensym (str "p" % "__")))
               (range 1 (inc max-idx))))
 
        nform (doall (map parse-autoarg form))
        args
        (concat 
         (gen-positional-params (:max *autofn-syms* 0)) 
         (when-let [r (*autofn-syms* 'rest)]
           (list '& r)))]
    `(fn* ~(vec args) ~nform)))

