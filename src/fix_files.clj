(ns fix-files
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [clojure.zip :as Z]
   [clojure.xml :as X]))

(defn parseInt [int-str]
  (try
    (Integer/parseInt int-str)
  (catch NumberFormatException e
    (println "Error parsing integer: " int-str)
    nil)))

(defn largest-idx [regexp dir]
  (let [files (filter
               #(re-find regexp (.getName %))
               (file-seq dir))]
    (if (seq files)
      (reduce
       (fn [result [type id]]
         (let [current (result type 0)
               maxi (fnil max 0)]
           (update result (keyword type) maxi id)))
       {}
       (map
        (fn [f]
          (let [parts (re-find regexp (.getName f))
                type (get parts 1)
                id (-> (get parts 2) parseInt)]
            [type (or id 0)]))
        files))
      nil)))

(defn create-in-files [dir in-f-format out-f-format re-shuffle]
  (filter
   (fn [f]
     (let [f-name (.getName f)]
       (and
        (re-find in-f-format f-name)
        ;(or re-shuffle
        ;    (not (re-matches out-f-format f-name)))
        )))
   (file-seq dir)))

(defn create-ext-regex-str [& exts]
  (reduce (fn [r e]
            (str r "|\\." e))
          (str "\\." (first exts))
          (rest exts)))

(defn fix-D-files
  ([dir val-percent re-shuffle]
   (fix-D-files dir val-percent re-shuffle "jpg" "png" "gif"))

  ([dir val-percent re-shuffle & exts]
   (let [dir (io/file dir)
         annons-dir (io/file dir "annotations")
         ext (apply create-ext-regex-str exts)
         inp-file-format (re-pattern (str "(.*)(" ext ")$"))
         out-file-format (re-pattern (format "(%s\\.|%s\\.)([0-9]+)\\-[0-9]+(%s)" "train" "val" ext))
         final-out-format (re-pattern (format "(%s\\.|%s\\.)([0-9]+)(%s)" "train" "val" ext))
         annon-file-format (re-pattern (format "(%s\\.|%s\\.)([0-9]+)\\-[0-9]+(\\.xml)" "train" "val"))
         val-file-format (re-pattern (format "(%s\\.)([0-9]+)\\-[0-9]+(%s)" "val" ext))
         train-file-format (re-pattern (format "(%s\\.)([0-9]+)\\-[0-9]+(%s)" "train" ext))
         in-files (create-in-files dir inp-file-format out-file-format re-shuffle)
         max-current-id (largest-idx out-file-format dir)]
     (if (seq in-files)
       (loop [train-id (:train. max-current-id 0)
              val-id (:val. max-current-id 0)
              fs in-files]
         (if-let [f (first fs)]
           (let [inp-file-name (.getName f)
                 [_ fname ext] (re-find inp-file-format inp-file-name)
                 annon-file (io/file annons-dir (format "%s.xml" fname))
                 [is-val? train-id val-id] (cond
                                             (re-matches val-file-format inp-file-name) [true train-id val-id]
                                             (re-matches train-file-format inp-file-name) [false train-id val-id]
                                             :OTHERWISE (if (< (rand-int 100) val-percent)
                                                          [true train-id (inc val-id)]
                                                          [false (inc train-id) val-id]))
                 [_ ofname oid oext] (re-find out-file-format inp-file-name)
                 out-file-name (cond
                                 (or (re-matches val-file-format inp-file-name)
                                     (re-matches train-file-format inp-file-name)) (format "%s%s%s" ofname oid oext)
                                 is-val? (format "val.%06d%s" val-id ext)
                                 (not is-val?) (format "train.%06d%s" train-id ext))
                 ]
             (.renameTo f (io/file dir out-file-name))
             (when (.exists annon-file)
               (let [[_ name id ext] (re-find final-out-format out-file-name)
                     out-annon-name (format "%s%s.xml" name id)]
                 (.renameTo annon-file (io/file annons-dir out-annon-name))))
             (recur train-id val-id (rest fs)))))))))


;(fix-D-files "./data/zacatecas-DS/test" "zac")
