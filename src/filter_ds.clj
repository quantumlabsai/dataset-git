(ns filter-ds
  (:require
   [clojure.pprint :as pp]
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.java.io :as io]
   [clojure.string :as S]
   ;[create-csv]
   [ds.core :as core]
   )
  ;(:gen-class)
  )

(defn read-filtered-csv-labels [clases csvf]
  (let [lines (with-open [rdr (io/reader csvf)]
                (->> (line-seq rdr)
                     (map (fn [line]
                            (let [[img xmin xmax ymin ymax clase] (S/split line #",")]
                              [img xmin xmax ymin ymax clase])))
                     (filter (fn [[_ _ _ _ _ clase]]
                               (clases clase)))
                     (doall)))]
    lines))

(defn write-csv-labels [root name lbls]
  (with-open [out (io/writer (io/file root name))]
    (doseq [[img xmin xmax ymin ymax cls] lbls]
      (.write out (format "%s,%s,%s,%s,%s,%s\n" img xmin xmax ymin ymax cls)))))

(defn do-filter-ds [in out clases]
  (let [clases (into #{} clases)
        in (io/file in)
        out (io/file out)
        train-lbls (read-filtered-csv-labels clases (io/file in "labels-train.csv"))
        val-lbls (read-filtered-csv-labels clases (io/file in "labels-val.csv"))
        train-imgs (into #{} (map first train-lbls))
        val-imgs (into #{} (map first val-lbls))]
    (println "creating dir: " (.getCanonicalPath out))
    (.mkdirs out)
    (println "train-lbls: " (count train-lbls) " train-imgs: " (count train-imgs))
    (println "val-lbls  : " (count val-lbls) " val-imgs:   " (count val-imgs))
    (println "copying ...")
    (doseq [img (concat train-imgs val-imgs)]
      (io/copy (io/file in img) (io/file out img)))
    (write-csv-labels out "labels-train.csv" train-lbls)
    (write-csv-labels out "labels-val.csv" val-lbls)
    (println "done")))

(def cli-options
  ;; An option with a required argument
  [["-h" "--help"]
   ["-c" "--clases list" "lista de clases separada por comas a conservar en le dataset de salida"]
   ["-i" "--in dir" "In dataset directory"
    :validate [(fn [arg]
                 (println "arg:" arg)
                 (let [dir (io/file arg)]
                   (println (.getCanonicalPath dir))
                   (and (.exists dir) (.isDirectory dir)))) "Directorio debe existir"]]
   ["-o" "--out dir" "In dataset directory"
    :validate [(fn [arg]
                 (println "arg:" arg)
                 (let [dir (io/file arg)]
                   (println (.getCanonicalPath dir))
                   (or (not (.exists dir))
                       (.isDirectory dir)))) "Directorio no debe existir o ser un directorio"]]])


(defn -main [& args]
  (println "Iniciando proceso de generación de sub conjunto de un dataset!!!")
  (try
    (let [{:keys [options arguments errors summary] :as m} (parse-opts args cli-options)]
      (when (seq errors)
        (doseq [err errors]
          (println err))
        (System/exit 1))
      (let [{:keys [clases in out]} options]
        (println "Options:")
        (pp/pprint (into (sorted-map) options))
        (if (:help options)
          (println (:summary m))
          (let [_ (println "clases:" clases)
                clases (S/split clases #",")]
            (do-filter-ds in out clases)))))
  (catch Throwable e
    (println "Process terminated with error " e)
    (System/exit 1))))
