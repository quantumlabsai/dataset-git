(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :as S]
            [clojure.xml :as X]
            [clojure.zip :as Z]
            [clojure.java.shell :as shell])
  (:import (java.io File)))

(def ds (io/file "/Users/felipe/Downloads/udacity_driving_datasets/"))
(def an (io/file ds "annotations"))
(def fs (vec (.listFiles an)))
(def names (vec (map #(.getName %) fs)))
(def dest (io/file "/Users/felipe/Projects/Python/pocs/placas/data/zacatecas-DS/UDACITY"))

(comment defn copy-ds []
  (doseq [n  names]
    (let [n2 (str (first (S/split n #"\.")) ".jpg")
          s (io/file ds n2)
          d (io/file (io/file "/Users/felipe/Projects/Python/pocs/placas/data/zacatecas-DS/UDACITY") n2)]
      (io/copy s d)
      (println n2))))

(defn move-train2val [train-dir val-dir percent]
  (let [train-dir (io/file train-dir)
        val-dir (io/file val-dir)
        train-files (vec (.listFiles train-dir))
        val-files (random-sample percent train-files)]
    (doseq [val val-files]
      (let [annon-dir (io/file (.getParent val) "annotations")
            annon-name (str (reduce str (butlast (S/split (.getName val) #"\."))) ".xml")
            src-annon (io/file annon-dir annon-name)
            val-annon-dir (io/file val-dir "annotations")
            dest-annon (io/file val-annon-dir annon-name)
            dest-val (io/file val-dir (.getName val))]
        (println "move:" (.getCanonicalPath val) " -> " (.getCanonicalPath dest-val))
        (.renameTo val dest-val)
        (when (.exists src-annon)
          (println "move:" (.getCanonicalPath src-annon) " -> " (.getCanonicalPath dest-annon))
          (.renameTo src-annon dest-annon))))))

(defn name-no-ext [name]
  (let [parts (S/split name #"\.")]
    (if (and (seq parts) (> (count parts) 1))
      (reduce (fn [r p]
                (str r "." p))
              (butlast (S/split name #"\.")))
      name)))

(defn create-ext-regex-str [& exts]
  (reduce (fn [r e]
            (str r "|\\." e))
          (str "\\." (first exts))
          (rest exts)))

(defn name&ext [f]
  (let [parts (S/split (.getName f) #"\.")]
    [(str (reduce (fn [r p]
                    (str r "." p))
                  (first parts)
                  (rest (butlast parts))))
     (last parts)]))

(defn copy-f&annon [s-file dest-dir prefix idx]
  (let [s-dir (.getParentFile s-file)
        source-annons (io/file s-dir "annotations")
        [name ext] (name&ext s-file)
        s-annon-name (str name ".xml")
        s-annon-file (io/file source-annons s-annon-name)
        rnd (rand-int 100)
        backgrownd? (or (not (.exists s-annon-file)) (= 0 (object-count s-annon-file)))
        copy? (or (not backgrownd?) (<= rnd background-percent))]
    (if copy?
      (try
        (if (.exists s-annon-file)
          (X/parse s-annon-file))
      (catch Exception e
        (println "Problemas en el xml: " (.getAbsolutePath s-annon-file)))))
    (if copy?
      (let [
            d-file (io/file dest-dir (format "%s.%06d.%s" prefix idx ext))
            [d-name d-ext] (name&ext d-file)
            d-annon-name (str d-name ".xml")
            d-annon-file (io/file dest-annons d-annon-name)]
        (io/copy s-file d-file)
        (when (.exists s-annon-file)
          (io/copy s-annon-file d-annon-file))
        (recur (rest s-files) (inc idx)))
      (recur (rest s-files) idx)))
  )

(defn recursive-copy-and-rename [source-dir prefix idx dest-dir background-percent exts]
  (let [source-annons (io/file source-dir "annotations")]
    (cond
      (.exists source-annons)
      (let [file-re (re-pattern (str ".*(" (apply create-ext-regex-str exts) ")"))
            dest-annons (io/file dest-dir "annotations")
            s-files (vec
                     (filter (fn [file]
                               (and (.isFile file) (re-matches file-re (.getName file))))
                             (.listFiles source-dir)))]
        (loop [s-files s-files idx idx]
          (if-let [s-file (first s-files)]
            (let [[name ext] (name&ext s-file)
                  s-annon-name (str name ".xml")
                  s-annon-file (io/file source-annons s-annon-name)
                  rnd (rand-int 100)
                  copy? (or (.exists s-annon-file) (<= rnd background-percent))]
              (if copy?
                (try
                  (if (.exists s-annon-file)
                    (X/parse s-annon-file))
                (catch Exception e
                  (println "Problemas en el xml: " (.getAbsolutePath s-annon-file)))))
              (if copy?
                (let [
                      d-file (io/file dest-dir (format "%s.%06d.%s" prefix idx ext))
                      [d-name d-ext] (name&ext d-file)
                      d-annon-name (str d-name ".xml")
                      d-annon-file (io/file dest-annons d-annon-name)]
                  (io/copy s-file d-file)
                  (when (.exists s-annon-file)
                    (io/copy s-annon-file d-annon-file))
                  (recur (rest s-files) (inc idx)))
                (recur (rest s-files) idx)))
            idx)))

      (.isDirectory source-dir)
      (loop [childs (filter #(.isDirectory %) (.listFiles source-dir)) idx idx]
        (if-let [child (first childs)]
          (let [idx (recursive-copy-and-rename child prefix idx dest-dir background-percent exts)]
            (recur (rest childs) idx))
          idx)))))

(defn merge-train-val-ds [in dest-dir background-percent & exts]
  (let [source-root-dir (io/file in)
        train-dir (io/file source-root-dir "train")
        val-dir (io/file source-root-dir "val")
        dest-dir (io/file dest-dir)
        dest-annons (io/file dest-dir "annotations")]
    (println "Creating dir:" (.getCanonicalPath dest-annons))
    (.mkdirs dest-annons)
    ;recorrer recursivamente train y compiar archivos renombrandolos train.nnnn al directorio temporal

    (let [n-train (recursive-copy-and-rename train-dir "train" 0 dest-dir background-percent exts)
          _ (println n-train " training images and annotations copied")
          n-val (recursive-copy-and-rename val-dir "val" 0 dest-dir background-percent exts)
          _ (println n-val " validation images and annotations copied")])))

(defn find-orphan-xml [dir]
  (loop [[f & files] (.listFiles dir) orphans []]
    (if f
      (cond
        (.isDirectory f)
        (let [orphans (find-orphan-xml f)]
          (recur files orphans))

        (re-matches #".*\.xml" (.getName f))
        (let [[name ext] (name&ext f)
              jpg-file (io/file (-> f .getParentFile .getParentFile) (str name ".jpg"))]
          (recur files (if (not (.exists jpg-file)) (conj orphans f) orphans)))

        :OTHERWISE
        (recur files orphans))
      orphans)))
