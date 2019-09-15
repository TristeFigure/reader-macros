(ns reader-macros.core-test
  (:require [clojure.test :refer :all]
            [reader-macros.core :refer :all]))

(defn rot13 [s]
  "Only works for lower case letters."
  (apply str (map #(char (+ (mod (+ (- (int %) 97) 13) 26) 97))
                  s)))

(defn macro-read-rot13
  [reader character _ _]
  (let [string (macro-read-string reader character)]
    (rot13 string)))

(set-macro-character \" macro-read-rot13)

(deftest test-rot13
  (is (= "uryyb" (apply str '(\h \e \l \l \o)))))
