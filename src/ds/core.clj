(ns ds.core
  (:require
   [clojure.pprint :as pp]
   [clojure.java.io :as io]
   [clojure.zip :as Z]
   [clojure.xml :as X]
   [clojure.string :as S]
   [clojure.java.shell :as sh]
   [fix-files :as fix]
   [img-util :as iu]
   [ds.util :as u]
   [ds-manager :refer [create-ds-info]])
  ;(:import
  ; (org.opencv.core Mat Size CvType)
  ; (org.opencv.imgcodecs Imgcodecs)
  ; (org.opencv.imgproc Imgproc))
  )

(def cls<->id (atom {:next 0
                     :id2cls ["background"]
                     :cls2id {"background" 0}}))

(defn update-registry [{:keys [next id2cls cls2id] :as registry} name]
  (if-not (cls2id name)
    (let [new-val {:next (inc next)
                   :id2cls (conj id2cls name)
                   :cls2id (assoc cls2id name (inc next))}]
      new-val)
    registry))

(defn register [name]
  (swap! cls<->id update-registry name))

(defn cls->id [name]
  (get-in @cls<->id [:cls2id name]))

(defn id->cls [id]
  (get-in @cls<->id [:id2cls id]))

(defn reduce2map [object & {:keys [content-extractor] :or {content-extractor identity}}]
  (reduce (fn [r {:keys [tag content]}]
            (assoc r tag (content-extractor content)))
          {}
          object))

(defn extract-bbox [class-fixer-fn object]
  (try
    (let [class-fixer-fn (or class-fixer-fn identity)
          obj-map (-> object :content reduce2map)
          {:keys [xmin xmax ymin ymax] :as bbox} (->
                                                  obj-map
                                                  :bndbox
                                                  (reduce2map :content-extractor #(Integer/parseInt (S/replace (first %) #"\n" ""))))
          name (-> obj-map :name first)
          name (if name (-> name (S/replace #"\n" "") class-fixer-fn))]
      (when name
        (register name)
        (assoc bbox :class name)))
  (catch NullPointerException e
    (.printStackTrace e))))

(defn get-clipping [height width [out-height out-width :as out-shape] top-clip left-clip]
  (let [razon-deseada (double (/ out-width out-height))
        razon-real (double (/ width height))]
    (if (or (< top-clip 0) (< left-clip 0))
      {:left-init 0 :right-init (int width)
       :top-init 0 :bottom-init (int height)
       :new-width width :new-height height}
      (if (> razon-real razon-deseada)
        (let [new-width (* height razon-deseada)
              eliminar-horizontal (- width new-width)
              left-init (int (/ (* eliminar-horizontal left-clip) 100))
              right-init (int (+ left-init new-width))]
          {:left-init left-init :right-init right-init
           :top-init 0 :bottom-init (int height)
           :new-width (int new-width) :new-height height})
        (let [new-height (/ width razon-deseada)
              eliminar-vert (- height new-height)
              top-init (int (/ (* eliminar-vert top-clip) 100))
              bottom-init (int (+ top-init new-height))]
          {:left-init 0 :right-init (int width)
           :top-init top-init :bottom-init bottom-init
           :new-width width :new-height (int new-height)})))))

(defn adjust-resize [{:keys [left-init top-init new-width new-height]}
                     [out-height out-width :as out-shape]
                     {:keys [xmin xmax ymin ymax] :as bbox}]
  (let [xmin (max 0 (- xmin left-init))
        xmax (max 0 (- xmax left-init))
        ymin (max 0 (- ymin top-init))
        ymax (max 0 (- ymax top-init))
        xmin (int (/ (* xmin out-width) new-width))
        xmax (int (/ (* xmax out-width) new-width))
        ymin (int (/ (* ymin out-height) new-height))
        ymax (int (/ (* ymax out-height) new-height))]
    (assoc bbox
           :xmin (int xmin) :xmax (int xmax)
           :ymin (int ymin) :ymax (int ymax))))

(defmulti fix-it (fn [{:keys [tag]} out-shape bbox class-fixer xml-file]
                   (or tag :default)))

(defmethod fix-it :default [node out-shape bbox class-fixer xml-file]
  node)

(defmethod fix-it :height [node [height width :as out-shape] bbox class-fixer xml-file]
  (assoc node :content [(str height)]))

(defmethod fix-it :width [node [height width :as out-shape] bbox class-fixer xml-file]
  (assoc node :content [(str width)]))

(defmethod fix-it :xmin [node out-shape {:keys [xmin xmax ymin ymax]} class-fixer xml-file]
  (assoc node :content [(str xmin)]))

(defmethod fix-it :xmax [node out-shape {:keys [xmin xmax ymin ymax]} class-fixer xml-file]
  (assoc node :content [(str xmax)]))

(defmethod fix-it :ymin [node out-shape {:keys [xmin xmax ymin ymax]} class-fixer xml-file]
  (assoc node :content [(str ymin)]))

(defmethod fix-it :ymax [node out-shape {:keys [xmin xmax ymin ymax]} class-fixer xml-file]
  (assoc node :content [(str ymax)]))

(defmethod fix-it :name [node out-shape bbox class-fixer xml-file]
  (let [current-cls (-> node :content first)
        fixed-cls (class-fixer current-cls)]
    (assoc node :content [fixed-cls])))

(defmethod fix-it :filename [node out-shape bbox class-fixer xml-file]
  (let [fname (.getName xml-file)
        len (count fname)
        fname (subs fname 0 (- len 4))
        img-name (str fname ".jpg")]
    (assoc node :content [img-name])))


(defn create-fixer [out-shape bboxs class-fixer xml-file]
  (letfn [(proc-me [z obj-idx]
                   (let [n (Z/node z)
                         bboxs (vec bboxs)]
                     [(Z/edit z fix-it out-shape (get bboxs (dec obj-idx)) class-fixer xml-file)
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

(defn adjust-xml [xml-file out-dir out-size bboxs class-fixer]
  (if (.exists xml-file)
    (let [out-xml-file (io/file out-dir (.getName xml-file))
          xml (X/parse xml-file)
          fixer (create-fixer out-size bboxs class-fixer xml-file)
          new-xml (fixer xml)
          out (java.io.StringWriter.)
          _ (binding [*out* out] ;(io/writer out-xml-file)
                     (X/emit new-xml))
          xml-str (-> out .toString (.replace "\n" ""))]
      (with-open [out (io/writer out-xml-file)]
        (.write out xml-str 0 (count xml-str))))))


(defn get-bbox [out-shape top-clip left-clip class-fixer [img-file annon-file]]
  (try
    (let [xml (if (.exists annon-file) (X/parse annon-file))
          obj-map (if xml (-> xml :content reduce2map))
          img (iu/image-read img-file)
          [width height depth] (iu/image-shape img)

          objects (if xml (->> xml :content (filter #(= (:tag %) :object))))
          bboxs (vec (filter identity (map (partial extract-bbox class-fixer) objects)))
          clipping (get-clipping height width out-shape top-clip left-clip)
          bboxs (map (partial adjust-resize clipping out-shape) bboxs)]
      [img-file clipping bboxs])
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

(defn captureORstop [continueRE]
  (when (not (re-matches continueRE (read-line)))
    (println "Abortando el proceso!!")
    (System/exit 1)))

(defn recreate-dir [dir force]
  (let [path (.getCanonicalPath dir)]
    (when (not force)
      (println "Se borrara el directorio: " path)
      (println "se va a ejecutar: rm -rf " path)
      (println "desea continuar? teclee [Si/No] :")
      (captureORstop #"[Ss][Ii]")
      (println "SEGURO ??? [Si/No]")
      (captureORstop #"[Ss][Ii]"))
    (sh/with-sh-dir
     "."
     (sh/sh "rm"  "-rf" path)
     (sh/sh "mkdir" "-p" (str path "/annotations")))))

(defn capture-int [msg min max]
  (println msg)
  (loop []
    (println "teclee un número entero entre " min " y " max " ?")
    (let [n (fix/parseInt (read-line))]
      (if (or (not n) (< n min) (> n max))
        (recur)
        n))))

(def top-clip-msg "

Si se tiene que recortar la imagen perdiendo a la vertical, qué porcentaje
de lo que se va a eliminar de la imagen desea que se pierda de arriba?
ej: si vamos a perder 300p a la verticar y desea que se pierda más bien
  2 tercios (200p) de lo de arriba y un tercio (100p) de lo de abajo
  conteste 66 (200 ~ 66 % de 300)
")

(def left-clip-msg "

Si se tiene que recortar la imagen perdiendo a la horizontal, qué porcentaje
de lo que se va a eliminar de la imagen desea que se pierda de la izquierda?
ej: si vamos a perder 300p a la horizontal y desea que se pierda la mitad
    (150p) de la izq y la otra mitad de la derecha
    conteste 50 (50%)
")

;(defn transform-DS [in-dir out-dir validation-percent out-shape top left force cls-id-edn-file annon-fix-file background-percent & exts]
;  (apply dir->csv in-dir out-dir validation-percent out-shape top left force cls-id-edn-file annon-fix-file background-percent exts))

(def tmp-ds-dir "__tmp-ds-dir__")

(defn all-classes-are-fine? [all-classes cls-id-edn class-fixer]
  (every? #(cls-id-edn %) (map class-fixer all-classes)))



(defn pre-commit [{:keys [prefix ext in cls2id annon-fix]}]
  (println "Removing error.log..")
  (.delete (io/file "error.log"))
  (try
    (let [root-dir (io/file in)
          ds-info (u/create-ds-info root-dir)]
      (u/print-ds-info ds-info)
      (let [annon-fix-fn (u/read&create-fix-fn (io/file annon-fix))
            cls2id-file (io/file cls2id)
            cls2id (if (and cls2id (.exists cls2id-file))
                     (read-string (slurp cls2id-file)))
            [all-classes problems] (u/extract-all-classes-recursively root-dir cls2id annon-fix-fn)
            all-classes-pp (vec (sort (into [] all-classes)))]
        (println "Classes found:")
        (pp/pprint all-classes-pp)
        (when (seq problems)
          (u/log-error "Unknown classes in xml files" problems))
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
