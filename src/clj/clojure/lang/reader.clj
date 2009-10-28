(ns clojure.lang.reader
  (:require (clojure.lang.reader [internal :as int]))
  (:refer-clojure :exclude (read)))

(def *eof* nil)

(def *black-hole* (proxy [clojure.lang.APersistentVector] []
                    (cons [obj] this)
                    (count [] 0)))

; (def *skip* (new [] (toString [] "Object ignored by reader"))) 
(def *skip* (Object. ))

(def *non-token-chars* (set (seq "()[]{}\\\"'`@~")))

(defn- whitespace? [char]
  (or (= char \,)
      (Character/isWhitespace char)))

(defn- breaks-token? [char]
  (or (whitespace? char)
      (*non-token-chars* char)))

(defn make-rh [r]
  (letfn [(add-char [ch [old-ch line offset]]
                    (if (= \newline old-ch)
                        [ch (inc line) 0 r]
                        [ch line (inc offset) r]))
          (make-rh-helper [rdr current-info]
                          (lazy-seq
                            (let [ch (.read rdr)]
                              (if (not (== ch -1))
                                (let [new-info (add-char (char ch) current-info)]
                                  (cons new-info
                                        (make-rh-helper rdr new-info)))
                                (list (add-char nil current-info))))))]
    (make-rh-helper r [\n 0 0])))

(defn- get-line [rh]
  (let [r (nth rh 3)]
    (if (instance? clojure.lang.LineNumberingPushbackReader r)
      (.getLineNumber #^clojure.lang.LineNumberingPushbackReader r)
      -1)))

(defn- get-position [rh]
  (let [[ch _ offset r] (first rh)]
    [(get-line rh) offset]))

(defn- reader-error [rh msg-str & objs]
  (let [[line offset] (get-position rh)]
    (throw (Exception. (apply format 
                              (str msg-str " (line %s, column %s)") 
                              (concat objs [line offset]))))))

(defn- advance 
  ([rh] (advance rh 1))
  ([rh n]
     (drop n rh)))

(defn- get-char [rh]
  (ffirst rh))

(defn- put-char!
  "Danger! If you use this you need to be certain that the reader-handle is the most recent one."
  [rh]
  (let [[ch _ _ #^java.io.PushbackReader r] (first rh)]
    (when ch
      (.unread r (int ch)))))

(defn- maybe-with-meta [rh object meta-map & [do-error?]]
  (cond 
    (instance? clojure.lang.IMeta object)
    (.withMeta #^clojure.lang.IObj object (merge ^object meta-map))
    do-error? (reader-error rh "Cannot attach metadata to %s" (class object))
    :else object))

(defn- attach-line-meta [rh object]
  (maybe-with-meta rh object {:line (get-line rh)}))

(let [dispatch
      {\( ::open-list
       \[ ::open-vector
       \{ ::open-map
       ;; the ::open* handlers consume the closing character,
       ;; so this dispatch should never see them.
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
  ;; closes over the dispatch table
  (defn consumer-dispatch [rh]
    (let [c (get-char rh)]
      (cond 
        (nil? c)        ::eof
        (whitespace? c) ::skip
        :else           (dispatch c ::token)))))

;;; works around some bootstrapping issues
(declare consume)
(when-not (.isBound #'consume)
  (defmulti consume consumer-dispatch))

(defn item-seq [rh]
 (remove #(identical? % *skip*) 
         (lazy-seq
           (when rh 
             (let [[item rh] (consume rh)]
               (cons item (item-seq rh)))))))

; Prefix macro dispatch
(let [dispatch-table
      {\' ::var
       \! ::shebang
       \_ ::ignore-form
       \{ ::open-set
       \^ ::metadata
       \" ::regex-pattern
       \< ::fail-reader
       \= ::reader-eval
       \( ::fn-shortcut}]

  (defn prefix-dispatch [rh]
    (dispatch-table (get-char (advance rh)) ::eof)))

(declare handle-prefix-macro)
(when-not (.isBound #'handle-prefix-macro)
  (defmulti handle-prefix-macro prefix-dispatch))

(defmethod consume ::macro-prefix [rh]
  (handle-prefix-macro rh))

;;; List, vector, set, map handling

(defn- add-or-skip [acc [item rh]]
  [(if (identical? *skip* item) acc (conj acc item))
   rh])

(defn- consume-delimited [rh end? acc transform]
  ((fn [[a h]] 
     (if-let [c (get-char h)]
       (cond
         (end? c) 
         [(transform a) h]
         (whitespace? c) (recur [a (advance h)])
         :else (let [[a nrh] (add-or-skip a (consume h))]
                 (recur [a (advance nrh)]))))) 
   [acc rh]))


(defmethod consume ::unexpected-closing [rh]
  (reader-error rh "Unexpected closing character %s" (get-char rh)))


(defmethod consume ::open-list [rh]
  (let [line (get-line rh)
        attach-it (fn [[item nrh]]
                    [(maybe-with-meta nrh item {:line line}) nrh])]
    (attach-it
     (consume-delimited (advance rh) 
                        #(= \) %)
                        []
                        #(apply list %)))))

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
  (letfn [(worker [[item nrh]]
                  (cond 
                    (seq? item) [(eval item) nrh]
                    (symbol? item) [(Class/forName (str item)) nrh]
                    ; nrh gives the error spot a in not quite the
                    ; right spot, but that should be okay.
                    :else (reader-error nrh "Unsupported #= form")))]
    (worker (consume (advance rh 2)))))

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
      (let [ch (get-char rh)]
        (cond 
          (= ch \\) 
          (do (.append sb (quote-or-error rh))
              (recur (advance rh 2)))
          (= ch \") 
          [(.intern (str sb)) rh]
          :else 
          (do (.append sb ch)
              (recur (advance rh))))))))

;;; Wrapping reader macros

(defn- consume-and-wrap [rh sym]
  (letfn [(wrap [[item rh]]
                [(list sym item) rh])]
    (wrap (consume rh))))  

(defmethod consume ::syntax-quote [rh]
  (consume-and-wrap (advance rh) 'clojure.core/syntax-quote))

(defmethod consume ::deref [rh]
  (consume-and-wrap (advance rh) 'clojure.core/deref))

(defmethod consume ::quote [rh]
  (consume-and-wrap (advance rh) 'quote))

(defmethod consume ::meta [rh]
  (consume-and-wrap (advance rh) 'clojure.core/meta))

(defmethod consume ::unquote [rh]
  (if (= \@ (get-char (advance rh 1)))
    (consume-and-wrap (advance rh 2) 'clojure.core/unquote-splicing)
    (consume-and-wrap (advance rh) 'clojure.core/unquote)))
              
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
      ::no-match)))

(defn- parse-number [string]
  (let [possible-matches 
        (for [[pat handler] +number-patterns+]
          (match-against pat string handler))]
    (first (remove (partial identical? ::no-match) 
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
  (loop [acc [] h rh]
    (let [c (get-char h)]
      (if (and c (not (breaks-token? c)))
        (recur (conj acc c) (advance h))
        (do
          (when (and c (breaks-token? c))
            (put-char! h)) ; need to unread it. state :-(
          [(apply str acc) h])))))
   
(defn- consume-token [rh]
  (let [[token-str nrh] (consume-token-string rh)]
    [(parse-token rh token-str) nrh]))

(defmethod consume ::token [rh]
  (consume-token rh))

(defmethod consume ::keyword [rh]
  (let [nc (get-char (advance rh))
        [rh autoqual?] (if (= nc \:) [(advance rh) true] [rh false]) 
        [token-str nrh] (consume-token-string (advance rh))]
    (if autoqual?
      [(keyword (int/resolve-symbol (symbol token-str))) nrh]
      [(keyword token-str) nrh])))

;; TODO

(defmethod handle-prefix-macro ::regex-pattern [rh]
  (let [sb (new java.lang.StringBuilder)]
    (loop [rh (advance rh 2)]
      (let [ch (get-char rh)]
        (cond 
          (and (= ch \\) (= (get-char (advance rh)) \"))
          (do (.append sb \")
              (recur (advance rh 2)))
          (= ch \") 
          [(java.util.regex.Pattern/compile (str sb)) rh]
          :else 
          (do (.append sb ch)
              (recur (advance rh))))))))

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
    (loop [[item nrh] (consume (advance rh))]
      (cond 
        (identical? *skip* item) (recur (consume (advance nrh)))
        (map? the-meta) 
        [(maybe-with-meta nrh item (merge
                                    {:line (get-line rh)}
                                    the-meta)
           :error-if-not-imeta) 
         nrh]
        (is-tag? the-meta) 
        [(maybe-with-meta nrh item {:line (get-line rh)
                                    :tag the-meta}
           :error-if-not-imeta) 
         nrh]
        :else (reader-error rh 
               "Metadata tag must be a string, a symbol or a keyword")))))

(defn ignore-rest-of-line [rh]
  (loop [r rh]
    (let [ch (get-char r)]
      (if (or (not ch) (= \newline ch))
        (do (put-char! r) ; we don't want to consume the newline
            r)
        (recur (advance r))))))

(defmethod consume ::line-comment [rh]
  (consume (ignore-rest-of-line rh)))

(defmethod handle-prefix-macro ::shebang [rh]
  (consume (ignore-rest-of-line rh)))

(defmethod handle-prefix-macro ::ignore-form [rh]
  (let [ignore (fn [[item rh]] [*skip* rh])]
    (ignore (consume (advance rh 2)))))

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
      [(str ch) rh]
      (consume-token-string rh))))

; There's a lot of duplication in these two functions... Perhaps clean
; up later.
(defn- octal-escape [rh string]
  (letfn [(pn []
              (try (Integer/parseInt (subs string 1) 8)
                   (catch NumberFormatException e)))]
    (if-let [ch (pn)]
      (cond
        (not (<= 0 ch 0377))
        (reader-error rh "Octal escape sequence must be in range [0, 377]")
        :else (char ch))
      (reader-error rh "Invalid octal character constant \\%s" string))))

(defn- unicode-escape [rh string]
  (letfn [(pn []
              (and (== 5 (count string))
                   (try (Integer/parseInt (subs string 1) 16)
                        (catch NumberFormatException e))))]
    (if-let [ch (pn)]
      (cond
        (<= 0xD800 ch 0xDFFF)
        (reader-error rh "Invalid character constant \\%s" string)
        :else
        (char ch))
      (reader-error rh "Invalid unicode escape \\%s" string))))

(defmethod consume ::character [rh]
  (let [[string nrh] (consume-character-escape (advance rh))
        ch
        (cond
          (== 1 (count string))
          , (first string)
          (= (first string) \u)
          , (unicode-escape rh string)
          (= (first string) \o)
          , (octal-escape rh string)
          :else
          , (or (lookup-character string)
                ;; just error out if the escape is invalid
                (reader-error rh "invalid character escape: \\%s" string)))]
    [ch nrh]))

(defmethod consume ::skip [rh]
  [*skip* rh])

(defmethod consume ::eof [rh] 
  [*eof* rh])

(defmethod handle-prefix-macro ::eof [rh]
  [*eof* rh])

(defn read [pbr eof-value]
  (let [rh (make-rh pbr)]
    (binding [*eof* eof-value]
      (loop [[item nrh] (consume rh)]
        (if (identical? item *skip*)
          (recur (consume (advance nrh)))
          item)))))
