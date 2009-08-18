(ns clojure.lang.reader.internal)

(def *autofn-syms* nil)

(defn- get-or-set! [index]
  (if-let [sym (*autofn-syms* index)]
    sym
    (let [sym (gensym (str "p" index "__"))]
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
    (= form '%) (get-or-set! 1)
    (= form '%&) (get-or-set! 'rest)
    (= (first (str form)) \%) 
    (try (get-or-set! (Integer/parseInt (apply str (rest (str form))))) 
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

