(ns ds-sample)

(require '[clojure.java.io :as io]
         )

(defn get-files [root-file name-re]
  (let [files (->> root-file
                   (.listFiles)
                   (filter #(re-matches name-re (.getName %)))
                   (into []))]
    files))

(def jpgs (get-files (io/file "train/voc2007") #".*jpg$"))

(def val-jpgs (random-sample 0.1 jpgs))

(println (format "train count: %d" (count jpgs)))

(Thread/sleep 3000)

(defn train2val [train-dir val-dir prob]
  (let [train-root (io/file train-dir)
        jpg-files (get-files train-root #".*jpg$")
        _ (println (format "found %d files in %s" (count jpg-files) (.getName train-root)))
        val-jpgs (random-sample prob jpg-files)
        val-root (io/file val-dir)
        train-annon-root (io/file train-root "annotations")
        val-root (io/file val-dir)
        val-annon-root (io/file val-root "annotations")]
    (println (.getCanonicalPath train-annon-root))
    (println (.getCanonicalPath val-annon-root))
    (println (format "moving %s images and annotations" (count val-jpgs)))
    (Thread/sleep 5000)
    (.mkdirs val-annon-root)
    (doseq [val-jpg val-jpgs]
      (let [[_ name ext] (re-find #"(.*)(\.jpg$)" (.getName val-jpg))
            _ (println (format "moving %s %s" name ext))
            annon-train (io/file train-annon-root (str name ".xml"))
            annon-val (io/file val-annon-root (str name ".xml"))]
        (.renameTo val-jpg (io/file val-root (.getName val-jpg)))
        (.renameTo annon-train annon-val)))))

(train2val "train/voc2007" "val/voc2007" 0.1)

(train2val "train/voc2012" "val/voc2012" 0.1)

(def jpgs (get-files (io/file "train/voc2007") #".*jpg$"))

(println (format "tran count: %d" (count jpgs)))
