(ns clojure.reader.ClojureReader
  (:require [clojure.lang.reader :as r])
  (:import (java.io PushbackReader))
  (:gen-class
   :methods [#^{:static true} [read [java.io.PushbackReader Object] Object]]))

(defn -read [pbr eof-value]
  (let [rh (r/make-rh pbr)]
    (binding [r/*eof* eof-value]
      (loop [[item nrh] (r/consume rh)]
        (if (identical? item r/*skip*)
          (recur (r/consume nrh))
          item)))))
