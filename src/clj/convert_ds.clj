(ns convert-ds
  (:require
   [clojure.pprint :as pp]
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.java.io :as io]
   [clojure.string :as S]
   ;[create-csv]
   [ds.img-util :as U]
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

(defn read-annon [next lbl name annon-dir [fromH fromW] [toH toW]]
  (let [good-classes-set {1 1 ; person -> person
                          2 3 ; rider -> bicicle
                          3 1 ;partial person -> person
                          }
        annon-f (io/file annon-dir (format "%06d.jpg.txt" name))]
    (with-open [annon-rdr (io/reader annon-f)]
      (->> (line-seq annon-rdr)
           rest
           (map (fn [line]
                  (let [[cls xmin ymin xmax ymax] (->> (S/split line #" ")
                                                       (map #(Integer/parseInt %)))
                        xfactor (float (/ toW fromW))
                        yfactor (float (/ toH fromH))
                        xmin (Math/round (* xmin xfactor))
                        xmax (Math/round (* xmax xfactor))
                        ymin (Math/round (* ymin yfactor))
                        ymax (Math/round (* ymax yfactor))]
                    [(format "%s.%05d.jpg" lbl next) xmin xmax ymin ymax (good-classes-set cls 0)])))
           (filter (fn [[_ _ _ _ _ cls]]
                     (good-classes-set cls)))
           doall))))

(defn append-annon [next csv-f lbl name annon-dir [fromH fromW] [toH toW]]
  (let [annons (read-annon next lbl name annon-dir [fromH fromW] [toH toW])]
    (with-open [csv-wrt (io/writer csv-f :append true)]
      (doseq [[img xmin xmax ymin ymax cls] annons]
        (.write csv-wrt (format "%s,%d,%d,%d,%d,%d\n" img xmin xmax ymin ymax cls))))))

(defn merge-datasets [current-cnt csv-f name-set images annon lbl out]
  (reduce
   (fn [next name]
     (let [orig-img-f (io/file images (format "%06d.jpg" name))
           orig-annon-f (io/file annon (format "%06d.jpg.txt" name))
           out-img-f (io/file out (format "%s.%05d.jpg" lbl next))
           _ (when-not (.exists orig-img-f)
               (println (.getCanonicalPath orig-img-f))
               (System/exit 0))
           img (U/image-read orig-img-f)
           [origH origW] (U/image-shape img)
           img-out (U/image-resize img 270 480)]
       (println "next: " next " name: " name)
       (U/image-write img-out out-img-f)
       (append-annon next csv-f lbl name annon [origH origW] [270 480])
       (inc next)))
   (inc current-cnt)
   name-set))

(defn do-create-ds [in out]
  (let [in (io/file in)
        images (io/file in "Images")
        annon (io/file in "Annotations")
        train (into #{} (with-open [rdr (io/reader (io/file in "train.txt"))]
                          (->> (line-seq rdr)
                               (map #(Integer/parseInt %))
                               doall)))
        val (into #{} (with-open [rdr (io/reader (io/file in "val.txt"))]
                          (->> (line-seq rdr)
                               (map #(Integer/parseInt %))
                               doall)))
        train-csv-f (io/file out "labels-train.csv")
        current-train-csv (read-filtered-csv-labels identity (io/file out "labels-train.csv"))
        _ (println "xxxx " (count current-train-csv) " " (.getCanonicalPath (io/file out "labels-train.csv")))
        _ (println "last: " (pr-str (-> current-train-csv
                                        last
                                        first
                                        (S/split #"\."))))
        last-current-train (-> current-train-csv
                               last
                               first
                               (S/split #"\.")
                               second
                               (Integer/parseInt))
        _ (println (format "last-current-train: [%s]" last-current-train))
        val-csv-f (io/file out "labels-val.csv")
        current-val-csv (read-filtered-csv-labels identity (io/file out "labels-val.csv"))
        last-current-val (-> current-val-csv
                             last
                             first
                             (S/split #"\.")
                             second
                             (Integer/parseInt))]
    (merge-datasets last-current-train
                    train-csv-f
                    train
                    images
                    annon
                    "train"
                    (io/file out))
    (merge-datasets last-current-val
                    val-csv-f
                    val
                    images
                    annon
                    "val"
                    (io/file out))
    (println "done")))

(def cli-options
  ;; An option with a required argument
  [["-h" "--help"]
   ["-i" "--in dir" "In dataset directory este debe tener subdirectorios Images, Annotations y archivos train.txt y val.txt"
    :validate [(fn [arg]
                 (println "arg:" arg)
                 (let [dir (io/file arg)
                       images (io/file dir "Images")
                       annon (io/file dir "Annotations")
                       train (io/file dir "train.txt")
                       val (io/file dir "val.txt")]
                   (println (.getCanonicalPath dir))
                   (and (.exists dir) (.isDirectory dir)
                        (.exists images) (.isDirectory images)
                        (.exists annon) (.isDirectory annon)
                        (.exists train) (.exists val)))) "Directorio debe existir"]]
   ["-o" "--out dir" "In dataset directory"
    :validate [(fn [arg]
                 (println "arg:" arg)
                 (let [dir (io/file arg)]
                   (println (.getCanonicalPath dir))
                   (or (not (.exists dir))
                       (.isDirectory dir)))) "Directorio no debe existir o ser un directorio"]]])


(defn -main [& args]
  (println "Iniciando proceso de migración de WiderPerson a formato quantum!!!")
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
          (let []
            (do-create-ds in out)))))
  (catch Throwable e
    (println "Process terminated with error " e)
    (System/exit 1))))
