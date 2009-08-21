(ns clojure.lang.reader
  (:require (clojure.lang.reader [internal :as int])))

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

(defn- get-position [[offset lines]]
  [offset (or (:line (first lines)) 'N/A)])

(defn- reader-error [rh msg-str & objs]
  (let [[offset line] (get-position rh)]
    (throw (Exception. (apply format 
                              (str msg-str " (line %s, column %s)") 
                              (concat objs [line offset]))))))

(defn numbered-line-seq [#^java.io.BufferedReader rdr]
  (map #(struct <line> %1 %2) (iterate inc 1) (line-seq rdr)))

(defn- advance 
  ([rh] (advance rh 1))
  ([[offset lines] n]
     (when (first lines)
       (let [c (count (:content (first lines)))
             new-off (+ offset n)]
         (cond
           ;(zero? c) [0 (rest lines)]
           (> new-off c) (recur [0 (rest lines)] (- (dec new-off) c))
           :else [new-off lines])))))

(defn- get-line [[offset lines]]
  (if-let [line (first lines)]
    (:line line)))

(defn- get-char [[offset lines]]
  (let [line (first lines) 
        content (:content line)]
    (when line 
      (if (or (empty? content) 
              (== offset (count content)))
        \newline
        (.charAt content offset)))))



(defn- maybe-with-meta [rh object meta-map & [do-error?]]
  (cond 
    (instance? clojure.lang.IMeta object)
    (with-meta object (merge (meta object) meta-map))
    do-error? (reader-error rh "Cannot attach metadata to %s" (class object))
    :else object))

(defn- attach-line-meta [rh object]
  (maybe-with-meta rh object {:line (get-line rh)}))

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
         \: ::keyword
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
         \" ::regex-pattern
         \< ::fail-reader
         \= ::reader-eval
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
  (reader-error rh "Unexpected closing character %s" (get-char rh)))

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

;;; Other delimited things
(defmethod handle-prefix-macro ::fail-reader [rh]
    (reader-error rh "Unreadable form"))

(defmethod handle-prefix-macro ::reader-eval [rh]
  (let [[item rh] (consume (advance rh 2))]
    (cond 
      (seq? item) [(eval item) rh]
      (symbol? item) [(Class/forName (str item)) rh]
      :else (reader-error rh "Unsupported #= form"))))

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
        ch (get-char rh)]
    (if-let [escaped (quotable ch)]
      escaped
      (reader-error rh "Unsupported escape character: \\%s" ch))))
    
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

(def +int-pattern+ #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)")
(def +float-pattern+ #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")
(def +ratio-pattern+ #"([-+]?[0-9]+)/([0-9]+)")

(defn- select-radix [[dec-m hex-m oct-m custom-radix other-m]]
  (cond 
    dec-m [dec-m 10]
    hex-m [hex-m 16]
    oct-m [oct-m 8]
    other-m [other-m (Integer/parseInt custom-radix)]
    :else [nil nil]))

(defn- int-handler [[_ sign zero & radix-info]]
  (if zero
    0
    (let [negate    (= sign "-")
          [s radix] (select-radix radix-info)]
      (when s
        (let [bn (BigInteger. s radix)]
          (clojure.lang.Numbers/reduce (if negate (.negate bn) bn)))))))

(defn- float-handler [[s bigval _ _ bigdec?]]
  (if bigdec?
    (BigDecimal. bigval)
    (Double/parseDouble s)))

(defn- ratio-handler [[_ numerator denominator]]
  (clojure.lang.Numbers/divide (BigInteger. numerator) 
                               (BigInteger. denominator)))

(def +number-patterns+ 
     {+int-pattern+ int-handler,
      +float-pattern+ float-handler,
      +ratio-pattern+ ratio-handler})

(defn- match-against [pattern string match-handler]
  (let [[matches? & parts :as match] (re-matches pattern string)]
    (if matches?
      (match-handler match)
      :no-match)))

(defn- parse-number [string]
  (let [possible-matches 
        (for [[pat handler] +number-patterns+]
          (match-against pat string handler))]
    (first (remove (partial identical? :no-match) 
                   possible-matches))))

(defn- digit? [ch]
  (Character/isDigit ch))

(defn- possible-number? [string]
  (or 
   (and (> (count string) 1) (#{\+, \-} (first string)) (digit? (second string)))
   (digit? (first string))))

(defn- parse-token [rh string]
  (condp = string
    "" (reader-error rh "Expected token, got nothing")
    "nil" nil
    "true" true
    "false" false
    (cond 
      (possible-number? string) (if-let [n (parse-number string)]
                                  n
                                  (reader-error rh "Invalid number: %s" string))
      :else (symbol string))))

(defn- consume-token-string [rh]
  (loop [acc [] rh rh]
    (let [c (get-char rh)]
      (if (and c (not (breaks-token? c)))
        (recur (conj acc c) (advance rh))
        [(apply str acc) rh]))))
   
(defn- consume-token [rh]
  (let [[token-str nrh] (consume-token-string rh)]
    [(attach-line-meta nrh (parse-token rh token-str)) nrh]))

(defmethod consume ::token [rh]
  (consume-token rh))

(defmethod consume ::keyword [rh]
  (let [nc (get-char (advance rh))
        [rh autoqual?] (if (= nc \:) [(advance rh) true] [rh false]) 
        [token-str nrh] (consume-token-string (advance rh))]
    (if autoqual?
      [(keyword (str *ns*) token-str) nrh]
      [(keyword token-str) nrh])))

;; TODO

(defmethod handle-prefix-macro ::regex-pattern [rh]
  (let [sb (new java.lang.StringBuilder)]
    (loop [rh (advance rh 2)]
      (let [ch (get-char rh)
            new-rh (advance rh)]
        (cond 
          (and (= ch \\) (= (get-char new-rh) \"))
          (do (.append sb \")
              (recur (advance new-rh)))
          (= ch \") 
          [(java.util.regex.Pattern/compile (str sb)) new-rh]
          :else 
          (do (.append sb ch)
              (recur new-rh)))))))

(defmethod handle-prefix-macro ::fn-shortcut [rh]
  (when int/*autofn-syms*
    (reader-error rh "nested #()'s are forbidden"))
  (binding [int/*autofn-syms* {}] 
    (consume-delimited (advance rh 2)
                       #(= \) %)
                       []
                       int/autofn*))) 

(defn- is-tag? [item]
  (or 
   (string? item)
   (keyword? item)
   (symbol? item)))

(defmethod handle-prefix-macro ::metadata [rh]
  (let [[the-meta rh] (if (= \{ (get-char (advance rh 2)))
                 (consume-delimited (advance rh 3)
                                    #(= \} %)
                                    []
                                    #(apply hash-map %))
                 (consume (advance rh 2)))]
    (loop [[item nrh] (consume rh)]
      (cond 
        (identical? *skip* item)  (recur (consume nrh))
        (map? the-meta) 
          [(maybe-with-meta nrh item the-meta :error-if-not-imeta) 
           nrh]
        (is-tag? the-meta) 
          [(maybe-with-meta nrh item {:tag the-meta} :error-if-not-imeta) 
           nrh]
        :else (reader-error rh 
               "Metadata tag must be a string, a symbol or a keyword")))))

(defmethod consume ::line-comment [rh]
  (let [[_ lines] rh]
    (consume [0 (rest lines)])))

(def #^{:private true} lookup-character
     {"newline"   \newline
      "tab"       \tab
      "backspace" \backspace
      "return"    \return
      "space"     \space
      "formfeed"  \formfeed}) ; TODO: add \uNNNN

(defn consume-character-escape [rh]
  (let [ch (get-char rh)]
    (if (breaks-token? ch)
      [(str ch) (advance rh)]
      (consume-token-string rh))))
 
(defmethod consume ::character [rh]
  (let [[string nrh] (consume-character-escape (advance rh))
        ch
        (cond
          (== 1 (count string))
          , (first string)
          :else
          , (or (lookup-character string)
                ;; just error out if the escape is invalid
                (reader-error rh "invalid character escape: \\%s" string)))]
    [ch nrh]))

(defmethod consume ::skip [rh]
  [*skip* (advance rh)])

(defmethod consume ::eof [rh] 
  [rh nil])

(defmethod handle-prefix-macro ::eof [rh]
  [rh nil])
