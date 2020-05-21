(ns ds.core
  (:require
   [clojure.pprint :as pp]
   [clojure.java.io :as io]
   [clojure.zip :as Z]
   [clojure.xml :as X]
   [clojure.string :as S]
   [clojure.java.shell :as sh]
   [ds.img-util :as iu]
   [ds.util :as u]))

(defn reduce2map [object & {:keys [content-extractor] :or {content-extractor identity}}]
  (try
    (reduce
     (fn [r {:keys [tag content]}]
       (assoc r tag (content-extractor content)))
     {}
     object)
  (catch Exception e
    (println "Problems in xml file: " (.getMessage e))
    nil)))

(defn extract-bbox [class-fixer-fn object]
  (let [class-fixer-fn (or class-fixer-fn identity)
        obj-map (-> object :content reduce2map)
        bbox (->
              obj-map
              :bndbox
              (reduce2map :content-extractor (fn [content]
                                               ;(println (pr-str content))
                                               (Math/round (Float/parseFloat (S/replace (first content) #"[\n ]+" ""))))))
        ;_ (pp/pprint bbox)
        name (-> obj-map :name first)
        name (if name (-> name (S/replace #"\n" "") class-fixer-fn))]
    (when (and name bbox)
      (assoc bbox :class name))))

;(defn adjust-resize [{:keys [left-init top-init new-width new-height]}
;                     [out-height out-width :as out-shape]
;                     {:keys [xmin xmax ymin ymax] :as bbox}]
;  (let [xmin (max 0 (- xmin left-init))
;        xmax (max 0 (- xmax left-init))
;        ymin (max 0 (- ymin top-init))
;        ymax (max 0 (- ymax top-init))
;        xmin (int (/ (* xmin out-width) new-width))
;        xmax (int (/ (* xmax out-width) new-width))
;        ymin (int (/ (* ymin out-height) new-height))
;        ymax (int (/ (* ymax out-height) new-height))]
;    (assoc bbox
;           :xmin (int xmin) :xmax (int xmax)
;           :ymin (int ymin) :ymax (int ymax))))

(defn adjust-resize [[in-height in-width ]
                     [out-height out-width]
                     {:keys [xmin xmax ymin ymax] :as bbox}]
  (let [xmin (max 0 xmin)
        xmax (max 0 xmax)
        ymin (max 0 ymin)
        ymax (max 0 ymax)
        xmin (int (/ (* xmin out-width) in-width))
        xmax (int (/ (* xmax out-width) in-width))
        ymin (int (/ (* ymin out-height) in-height))
        ymax (int (/ (* ymax out-height) in-height))]
    (assoc bbox
           :xmin xmin :xmax xmax
           :ymin ymin :ymax ymax)))


(defmulti fix-it (fn [{:keys [tag]} out-shape bbox class-fixer xml-file-name]
                   (or tag :default)))

(defmethod fix-it :default [node out-shape bbox class-fixer xml-file]
  node)

(defmethod fix-it :height [node [height width :as out-shape] _ _ _]
  (assoc node :content [(str height)]))

(defmethod fix-it :width [node [height width :as out-shape] _ _ _]
  (assoc node :content [(str width)]))

(defmethod fix-it :xmin [node out-shape {:keys [xmin xmax ymin ymax]} _ _]
  (assoc node :content [(str xmin)]))

(defmethod fix-it :xmax [node out-shape {:keys [xmin xmax ymin ymax]} _ _]
  (assoc node :content [(str xmax)]))

(defmethod fix-it :ymin [node out-shape {:keys [xmin xmax ymin ymax]} _ _]
  (assoc node :content [(str ymin)]))

(defmethod fix-it :ymax [node out-shape {:keys [xmin xmax ymin ymax]} _ _]
  (assoc node :content [(str ymax)]))

(defmethod fix-it :name [node out-shape bbox class-fixer _]
  (let [current-cls (-> node :content first)
        fixed-cls (class-fixer current-cls)]
    (assoc node :content [fixed-cls])))

(defmethod fix-it :filename [node out-shape bbox class-fixer xml-file-name]
  (let [fname xml-file-name
        len (count fname)
        fname (subs fname 0 (- len 4))
        img-name (str fname ".jpg")]
    (assoc node :content [img-name])))


(defn create-fixer [out-shape bboxs class-fixer xml-file-name]
  (letfn [(proc-me [z obj-idx]
                   (let [n (Z/node z)
                         bboxs (vec bboxs)]
                     [(Z/edit z fix-it out-shape (get bboxs (dec obj-idx)) class-fixer xml-file-name)
                      (if (= (:tag n) :object) (inc obj-idx) obj-idx)]
                     ))
          (walk-down [z obj-idx]
                     (if-let [z (Z/down z)]
                       (let [[z obj-idx] (walk-right z obj-idx)]
                         [(Z/up z) obj-idx])
                       [z obj-idx]))
          (walk-right [z obj-idx]
                      (let [[z obj-idx] (proc-me z obj-idx)
                            [z obj-idx] (walk-down z obj-idx)]
                        (if-let [z (Z/right z)]
                          (walk-right z obj-idx)
                          [z obj-idx])))]
    (fn [xml]
      (let [[z last-obj] (walk-right (Z/xml-zip xml) 0)]
        (Z/root z)))))

(defn adjust-xml [xml-file out-xml-file [out-height out-width] bboxs class-fixer]
  (if (.exists xml-file)
    (let [xml (X/parse xml-file)
          fixer (create-fixer [out-height out-width] bboxs class-fixer (.getName out-xml-file))
          new-xml (fixer xml)
          out (java.io.StringWriter.)
          _ (binding [*out* out] ;(io/writer out-xml-file)
                     (X/emit new-xml))
          xml-str (-> out .toString (.replace "\n" ""))]
      (with-open [out (io/writer out-xml-file)]
        (.write out xml-str 0 (count xml-str)))))
  true)

(defn get-bboxs [[in-height in-width] [out-height out-width] class-fixer annon-file drop-classes]
  (try
    (let [xml (if (.exists annon-file) (X/parse annon-file))
          objects (if xml (->> xml :content (filter #(= (:tag %) :object))))
          ;_ (println (pr-str objects))
          bboxs (if (seq (:content objects))
                  (->> objects
                       (map (partial extract-bbox class-fixer))
                       (remove #(drop-classes (:class %)))
                       (mapv (partial adjust-resize [in-height in-width] [out-height out-width]))))]
      bboxs)
  (catch Exception e
    (println "Error en xml file: " annon-file)
    nil)))

(defn get-files-matching [dir re]
  (filter
   (fn [f]
     (re-matches re (.getName f)))
   (file-seq dir)))

(defn -load-xmls [dir re]
  (let [files (get-files-matching dir re)]
    (filter identity
            (map (fn [file]
                   (try
                     [file (X/parse file)]
                     (catch Exception e
                       (println "Problema en xml: " file)
                       nil
                       )))
                 files))))

(def load-xmls (memoize -load-xmls))

(defn get-childs-with-tag [node Dtag]
  (filter
   (fn [{:keys [tag]}]
     (= tag Dtag))
   (:content node)))

(defn get-name [object]
  (-> (get-childs-with-tag object :name) first :content first (S/replace  #"\n" "")))

(defn extract-classes [xml]
  (let [objects (get-childs-with-tag xml :object)]
    (map get-name objects)))

(defn extract-all-classes [annons-dir]
  (let [file-re (re-pattern (str  ".*\\.xml$")) ;(str  "(train|val)\\.[0-9]+\\.xml$")
        xmls (load-xmls annons-dir file-re)]
    (reduce
     (fn [classes [file xml]]
       (try
         (into classes (extract-classes xml))
       (catch NullPointerException e
         (println "Problemas en las anotaciones del archivo: " (.getCanonicalPath file))
         classes))
       )
     #{}
     xmls)))


(defn all-classes-are-fine? [all-classes cls-id-edn class-fixer]
  (every? #(cls-id-edn %) (map class-fixer all-classes)))


(defn pre-commit [{:keys [prefix ext in cls2id annon-fix drop-classes]}]
  (println "Removing error.log..")
  (println "drop-classes: " drop-classes)
  (.delete (io/file "error.log"))
  (try
    (let [root-dir (io/file in)
          ds-info (u/create-ds-info root-dir)]
      (u/print-ds-info ds-info)
      (let [annon-fix-fn (u/read&create-fix-fn (io/file annon-fix))
            cls2id-file (io/file cls2id)
            cls2id (if (and cls2id (.exists cls2id-file))
                     (read-string (slurp cls2id-file)))
            drop-classes-file (and drop-classes (io/file drop-classes))
            drop-classes (if (and drop-classes (.exists drop-classes-file))
                           (->> (slurp drop-classes-file)
                                (read-string)
                                (into #{}))
                           #{})
            [all-classes problems] (u/extract-all-classes-recursively root-dir cls2id annon-fix-fn drop-classes)
            all-classes-pp (vec (sort (into [] all-classes)))]
        (println "Classes found:")
        (pp/pprint all-classes-pp)
        (when (seq problems)
          (u/log-error "Unknown classes in xml files" (map #(.getCanonicalPath %) problems)))
        (if (u/all-classes-are-fine? all-classes cls2id annon-fix-fn)
          (let [orphan (u/find-orphan-xml root-dir)]
            (if (seq orphan)
              (u/log-error "This xml files are orphan (with no corresponding .jpg)" orphan))
            (println "All fine, commit approved!"))
          (throw (Exception. (format "Commit rejected, can't narrow classes %s -> %s" all-classes-pp (vec (sort (keys cls2id)))))))))
  (catch Exception e
    (println (.getMessage e))
    (println "Allways see error.log for details")
    (System/exit 1))))

(defn recreate-dir [dir force]
  (let [path (.getCanonicalPath dir)]
    (sh/with-sh-dir
     "."
     (sh/sh "rm"  "-rf" path)
     (sh/sh "mkdir" "-p" (str path "/annotations")))))

(defn is-ds-dir? [dir]
  (let [annon-dir (io/file dir "annotations")
        with-jpg (some #(re-matches #".*\.jpg$" (.getName %)) (.listFiles dir))
        sub-dirs (count (filter #(.isDirectory %) (.listFiles dir)))
        with-annons (.exists annon-dir)
        is-leaf (or (= 0 sub-dirs) (and with-annons (= 1 sub-dirs)))
        it-is (and with-jpg is-leaf)]
    it-is))



;;;;;;;;;;;;;;;;;


(defn resize&write-jpg [img out-height out-width out-jpg-file]
  (try
    (let [ds-img (iu/image-resize img out-height out-width)]
      (iu/image-write ds-img out-jpg-file)
      true)
  (catch Exception e
    (println (.getMessage e))
    (println "Problems resizing/writing image file: " (.getCanonicalPath out-jpg-file) " skiping"))))

(defn write2cvs-annon-file [include-background-in-csv out-dir prefix jpg-file-name boxes cls2id]
  (with-open [out (io/writer (io/file out-dir (str "labels-" prefix ".csv")) :append true)]
             (binding [*out* out]
                      (if (seq boxes)
                        (doseq [{:keys [class xmin xmax ymin ymax]} boxes]
                          (println (format "%s,%d,%d,%d,%d,%d" jpg-file-name xmin xmax ymin ymax (cls2id class))))
                        (if include-background-in-csv
                          (println (format "%s,%d,%d,%d,%d,%d" jpg-file-name 0 0 0 0 0)))))
             true))

(defn create-dataset-fn [out-dir prefix background-percent include-background-in-csv out-height out-width cls2id annon-fix-fn drop-classes]
  (let [out-annon-dir (doto (io/file out-dir "annotations") (.mkdirs))]
    (fn dataset-creator [in-dir n is-ds-dir]
      (loop [[file & files] (.listFiles in-dir) n n]
        (cond
          (nil? file)
          n

          (.isDirectory file)
          (recur files (dataset-creator file n (is-ds-dir? file)))

          (and (re-matches #".*\.jpg$" (.getName file)) is-ds-dir)
          (let [annon-dir (io/file (.getParentFile file) "annotations")
                [name ext] (u/name&ext file)
                annon-file (io/file annon-dir (str name ".xml"))
                out-name (format "%s.%05d" prefix n)
                out-jpg-file (io/file out-dir (str out-name ".jpg"))
                out-annon-file (io/file out-annon-dir (str out-name ".xml"))
                img (iu/image-read file)
                [img-height img-width  depth :as img-shape] (and img (iu/image-shape img))
                boxes (get-bboxs [img-height img-width] [out-height out-width] annon-fix-fn annon-file drop-classes)
                include-it? (and img
                                 (or
                                  (and (.exists annon-file)
                                       (> (count boxes) 0))
                                  (>= background-percent (rand-int 100))))]
            (if include-it?
              (if (adjust-xml annon-file out-annon-file [out-height out-width] boxes annon-fix-fn)
                (if (resize&write-jpg img out-height out-width out-jpg-file)
                  (let [n (inc n)]
                    (when (= 0 (mod n 10))
                      (if (= 0 (mod n 1000))
                        (println n)
                        (print "."))
                      (flush))
                    (write2cvs-annon-file include-background-in-csv out-dir prefix (.getName out-jpg-file) boxes cls2id)
                    (recur files n))
                  (recur files n))
                (recur files n))
              (recur files n)))

          :OTHERWISE
          (recur files n))))))

(defn post-commit [{:keys [prefix ext in out height width top left
                           cls2id annon-fix drop-classes background-percent
                           include-background-in-csv]}]
  (let [cls2id-file (io/file cls2id)
        cls2id (if (and cls2id (.exists cls2id-file))
                     (read-string (slurp cls2id-file)))
        annon-fix-fn (u/read&create-fix-fn (io/file annon-fix))
        drop-classes-file (and drop-classes (io/file drop-classes))
        drop-classes (if (and drop-classes (.exists drop-classes-file))
                       (->> (slurp drop-classes-file)
                            (read-string)
                            (into #{}))
                       #{})
        in-dir (io/file in)
        out-dir (io/file (format out prefix))
        out-annon-dir (io/file out-dir "annotations")]
    (try
      (doseq [branch ["train" "val"]]
        (let [creator (create-dataset-fn out-dir branch background-percent include-background-in-csv height width cls2id annon-fix-fn drop-classes)]
          (println (creator (io/file in-dir branch) 0 false))))
      (catch Exception e
        (println "\n\n******* ERROR ******** : " (.getMessage e))
        (.printStackTrace e)
        (System/exit 1)))))
