(ns create-xml-from-csv
  (:require [clojure.java.io :as io]
           [clojure.pprint :as pp]
           [clojure.string :as S]
           [clojure.tools.cli :refer [parse-opts]]))



(defn read-one-label [result line]
  (let [[img xmin xmax ymin ymax cls] (S/split line #",")
        result (if (= cls "0")
                 result
                 (update result img conj
                         [(Integer/parseInt  xmin) (Integer/parseInt  xmax)
                          (Integer/parseInt  ymin) (Integer/parseInt  ymax) (Integer/parseInt  cls)]))]
    result))

(defn load_csvs [root-dir label-files]
  (let [root (io/file root-dir)]
    (apply merge {}
     (map (fn [label-file]
            (let [csv-file (io/file root label-file)
                  _ (println "reading:" (.getCanonicalPath csv-file))
                  lines (line-seq (io/reader csv-file))]
              (reduce
               read-one-label
               {}
               lines)
              )) 
          label-files))))

(def HEAD ["<?xml version='1.0' encoding='UTF-8'?>"
           "<annotation>"
           "  <folder>COLLAPSED</folder>"
           "  <filename>%s</filename>"
           "  <size>"
           "    <width>480</width>"
           "    <height>270</height>"
           "    <depth>3</depth>"
           "  </size>"])

(def OBJECT ["  <object>"
             "    <name>%s</name>"
             "    <difficult>0</difficult>"
             "    <bndbox>"
             "      <xmin>%d</xmin><xmax>%d</xmax><ymin>%d</ymin><ymax>%d</ymax>"
             "    </bndbox>"
             "  </object>"])

(def TAIL ["</annotation>"])

(defn write-xml
  ([xml strs]
   (write-xml xml strs [nil]))
  ([xml strs data]
   (dorun
    (for [d data s strs]
      (let [s (str s "\n")]
        (if (re-find #"%" s)
          (.write xml (apply format s d))
          (.write xml (str s))))))))

(defn write-xml2 [xml annons]
  (doseq [annon annons]
    (let [annon (vec annon)]
      (println annon)
      (.write xml "  <object>\n")
      (.write xml (format "    <name>%s</name>\n" (annon 4)))
      (.write xml "    <difficult>0</difficult>\n")
      (.write xml "    <bndbox>\n")
      (.write xml (format "      <xmin>%d</xmin><xmax>%d</xmax><ymin>%d</ymin><ymax>%d</ymax>\n" (annon 0) (annon 1) (annon 2) (annon 3)))
      (.write xml "    </bndbox>\n")
      (.write xml "  </object>\n"))))

(defn write-annon [annotation-map path]
  (let [out-dir (io/file path)]
    (doseq [[img annons] annotation-map]
      (with-open [xml (io/writer (io/file out-dir (str  (subs img 0 (- (count img) 3)) "xml")))]
        (write-xml xml HEAD [[img]])
        (write-xml2 xml annons)
        (write-xml xml TAIL)))))

(def cli-options
  ;; An option with a required argument
  [["-h" "--help"]
   ["-i" "--in-dir DIR" "Input images directory"
    :validate [(fn [arg]
                 (let [dir (io/file arg)]
                   (println ":" (.getCanonicalPath dir))
                   (and (.exists dir) (.isDirectory dir)))) "Directorio debe existir"]]
   ])

(defn -main [& args]
  (println "starting xml generation...")
  (try
    (let [{:keys [options arguments errors summary] :as m} (parse-opts args cli-options)]
      (when (seq errors)
        (doseq [err errors]
          (println err))
        (System/exit 1))
      (let [{:keys [help in-dir]} options]
        (println "Options: " in-dir)
        (pp/pprint (into (sorted-map) options))
        (println "--->" (.getCanonicalPath (io/file in-dir)))
        (if help
          (println (:summary m))
          (let [labels (load_csvs in-dir ["labels-train.csv" "labels-val.csv"])]
            (write-annon labels (io/file in-dir "annotations"))))))
    (catch Throwable t
      (println (.getMessage t))
      (println t))))
