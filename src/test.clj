(require '[clojure.pprint :as pp])
(require '[clojure.java.io :as io])

(def err (read-string (subs (slurp "error.log") 29)))

(doseq [f-name err]
  (let [f (io/file f-name)
        cnt (count (.getName f))
        jpg-name (str (subs (.getName f) 0 (- cnt 3)) "jpg")]
    (let [parent (.getParentFile (.getParentFile f))
          jpg (io/file parent jpg-name)]
      (println (.getName jpg) " " (.getName f))
      (.delete jpg)
      (.delete f))))
