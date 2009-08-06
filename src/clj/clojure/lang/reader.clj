(ns clojure.lang.reader)

(defstruct <line> :line :content)

(def *black-hole* (proxy [clojure.lang.APersistentVector] []
                    (cons [obj] this)
                    (count [] 0)))

; (def *skip* (new [] (toString [] "Object ignored by reader"))) 
(def #^{:private true} *skip* (Object. ) )

(def *non-token-chars* (set (seq "()[]{}\\\"")))

(defn- whitespace? [char]
  (or (= char \,)
      (Character/isWhitespace char)))

(defn- breaks-token? [char]
  (or (whitespace? char)
      (*non-token-chars* char)))

(defn- reader-error [msg-str & objs]
  (throw (Exception. (apply format msg-str objs))))

(defn numbered-line-seq [#^java.io.BufferedReader rdr]
  (map #(struct <line> %1 %2) (iterate inc 1) (line-seq rdr)))

(defn- advance 
  ([rh] (advance rh 1))
  ([[offset lines] n]
     (when (first lines)
       (let [c (count (:content (first lines)))
             new-off (+ offset n)]
         (cond
           (zero? c) [0 (rest lines)]
           (>= new-off c) (recur [0 (rest lines)] (- new-off c))
           :else [new-off lines])))))
  
(defn- get-char [[offset lines]]
  (when (and (first lines) (not (empty? (:content (first lines)))))
    (.charAt (:content (first lines)) offset)))

(defn- get-position [[offset lines]]
  [offset (or (:line (first lines)) 'N/A)])

(defn consumer-dispatch [rh]
  (let [c (get-char rh)
       
        dispatch 
        {\( ::open-list
         \[ ::open-vector
         \{ ::open-map
         ; the ::open* handlers consume the closing character,
         ; so this dispatch should never see them.
         \" ::string
         \) ::unexpected-closing
         \] ::unexpected-closing
         \} ::unexpected-closing
         \@ ::deref
         \' ::quote
         \^ ::meta
         \` ::syntax-quote
         \~ ::unquote
         \# ::macro-prefix
         \\ ::character
         \; ::line-comment}]
    (cond 
      (nil? c)        ::skip
      (whitespace? c) ::skip
      :else           (dispatch c ::token))))      

(defmulti consume consumer-dispatch)

(defn item-seq [rh]
 (remove #(identical? % *skip*) 
         (lazy-seq
           (when rh 
             (let [[item rh] (consume rh)]
               (cons item (item-seq rh)))))))

; Prefix macro dispatch

(defn prefix-dispatch [rh]
  (let [dispatch-table
        {\' ::var
         \{ ::open-set
         \^ ::metadata
         \( ::fn-shortcut}]
    (dispatch-table (get-char (advance rh)) ::eof)))

(defmulti handle-prefix-macro prefix-dispatch)

(defmethod consume ::macro-prefix [rh]
  (handle-prefix-macro rh))

;;; List, vector, set, map handling

(defn consume-delimited [rh end? acc transform]
  ((fn [acc h] 
     (if-let [c (get-char h)]
       (if (end? c) 
         [(transform acc) (advance h)]
         (let [[item new-rh] (consume h)]
           (recur (if (identical? *skip* item) 
                    acc 
                    (conj acc item)) new-rh))))) 
   acc rh))

(defmethod consume ::unexpected-closing [rh]
  (let [[offset line] (get-position rh)]
    (reader-error "Unexpected closing character %s (line %s, character %s)" 
                  (get-char rh) line (inc offset))))

(defmethod consume ::open-list [rh]
  (consume-delimited (advance rh) 
                     #(= \) %)
                     []
                     #(apply list %)))

(defmethod consume ::open-vector [rh]
  (consume-delimited (advance rh) 
                     #(= \] %)
                     []
                     identity))

(defmethod consume ::open-map [rh]
  (consume-delimited (advance rh)
                     #(= \} %)
                     []
                     #(apply hash-map %)))

(defmethod handle-prefix-macro ::open-set [rh]
  (consume-delimited (advance rh 2)
                     #(= \} %)
                     #{}
                     identity))

;;; Strings

(defn quote-or-error [rh]
  ; TODO: Any missing?
  (let [quotable {\" \"  
                  \\ \\ 
                  \n \newline 
                  \t \tab 
                  \r \return 
                  \f \formfeed}
        rh (advance rh)
        ch (get-char rh)
        [offset line] (get-position rh)]
    (if-let [escaped (quotable ch)]
      escaped
      (reader-error "Unsupported escape character: \\%s (line %s, character %s)" 
                    ch line offset))))
    
(defmethod consume ::string [rh]
  (let [sb (new java.lang.StringBuilder)]
    (loop [rh (advance rh)]
      (let [ch (get-char rh)
            new-rh (advance rh)]
        (cond 
          (= ch \\) 
          (do (.append sb (quote-or-error rh))
              (recur (advance new-rh)))
          (= ch \") 
          [(str sb) new-rh]
          :else 
          (do (.append sb ch)
              (recur new-rh)))))))

;;; Wrapping reader macros

(defn- consume-and-wrap [rh symbol]
  (let [[item new-rh] (consume rh)]
    [(list symbol item) new-rh]))  

(defmethod consume ::syntax-quote [rh]
  (consume-and-wrap (advance rh) `syntax-quote))

(defmethod consume ::deref [rh]
  (consume-and-wrap (advance rh) `deref))

(defmethod consume ::quote [rh]
  (consume-and-wrap (advance rh) 'quote))

(defmethod consume ::meta [rh]
  (consume-and-wrap (advance rh) `meta))

(defmethod consume ::unquote [rh]
  (if (= \@ (get-char (advance rh 1)))
    (consume-and-wrap (advance rh 2) `unquote-splicing)
    (consume-and-wrap (advance rh) `unquote)))
              
(defmethod handle-prefix-macro ::var [rh]
  (consume-and-wrap (advance rh 2) 'var))
 
;;; Token handling

(defn- parse-token [rh string]
  (let [[offset line] (get-position rh)]
    (condp = string
      "" (reader-error "Expected token, got nothing. (line %s, character %s)" line offset)
      "nil" nil
      "true" true
      "false" false
      (symbol string))))

(defn consume-token [rh]
  (loop [acc [] h rh]
    (let [c (get-char h)]
      (if (and c (not (breaks-token? c)))
        (recur (conj acc c) (advance h))
        [(parse-token h (apply str acc)) h])))) 

(defmethod consume ::token [rh]
  (consume-token rh))


;; TODO

(defmethod handle-prefix-macro ::fn-shortcut [rh]
  (consume-and-wrap (advance rh) 'FN)) 

(defmethod handle-prefix-macro ::metadata [rh]
  (let [[_ rh] (if (= \{ (get-char (advance rh 2)))
                 (consume-delimited (advance rh 3)
                                    #(= \} %)
                                    *black-hole*
                                    identity)
                 (consume-token (advance rh 2)))]
    (loop [[item nrh] (consume rh)]
      (if (identical? *skip* item) 
        (recur (consume nrh))
        [item nrh]))))

(defmethod consume ::line-comment [rh]
  (let [[_ lines] rh]
    (consume [0 (rest lines)])))

;; not quite ok...
(defmethod consume ::character [rh]
  [(get-char (advance rh)) (advance rh 2)])

(defmethod consume ::skip [rh]
  [*skip* (advance rh)])

(defmethod consume ::eof [rh] 
  [rh nil])

(defmethod handle-prefix-macro ::eof [rh]
  [rh nil])
