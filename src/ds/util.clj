(ns ds.util
  (:require [clojure.zip :as Z]
            [clojure.xml :as X]
            [clojure.java.io :as io]
            [clojure.string :as S]
            [clojure.set :as set]
            [clojure.pprint :as pp]
            [clojure.java.shell :refer [sh]]))

(defn log-error
  ([msg]
   (binding [*out* (io/writer "error.log" :append true)]
            (println msg)
            (println)))
  ([msg coll]
   (binding [*out* (io/writer "error.log" :append true)]
            (println msg)
            (pp/pprint coll)
            (println))))

(defn log-error&throw
  ([msg]
   (log-error msg)
   (throw (Exception. msg)))
  ([msg coll]
   (log-error msg coll)
   (throw (Exception. msg))))

(defn name&ext [f]
  (let [name (.getName f)
        dot (.lastIndexOf name ".")]
    [(subs name 0 dot) (subs name (inc dot))]))


(defn find-orphan-xml [dir]
  (loop [[f & files] (.listFiles dir) orphans []]
    (if f
      (cond
        (.isDirectory f)
        (let [orphans (concat orphans (find-orphan-xml f))]
          (recur files orphans))

        (re-matches #".*\.xml" (.getName f))
        (let [[name ext] (name&ext f)
              jpg-file (io/file (-> f .getParentFile .getParentFile) (str name ".jpg"))]
          (recur files (if (not (.exists jpg-file)) (conj orphans f) orphans)))

        :OTHERWISE
        (recur files orphans))
      orphans)))


(defn get-files-matching [dir re]
  (filter
   (fn [f]
     (re-matches re (.getName f)))
   (file-seq dir)))

(defn parse-xmls [dir]
  (let [files (get-files-matching dir #".*\.xml$")]
    (map (fn [file]
           (try
             [file (X/parse file)]
             (catch Exception e
               [file nil])))
         files)))

(defn get-childs-with-tag [node Dtag]
  (filter
   (fn [{:keys [tag]}]
     (= tag Dtag))
   (:content node)))

(defn get-object-name [object]
  (if-let [name-str (-> (get-childs-with-tag object :name) first :content first)]
    (S/replace name-str #"\n" "")))

(defn extract-classes [xml]
  (let [objects (get-childs-with-tag xml :object)]
    (map get-object-name objects)))

(defn all-classes-are-fine? [all-classes cls-id-edn class-fixer]
  (every? #(cls-id-edn %) (map class-fixer all-classes)))

(defn extract-all-classes [annons-dir cls2id annon-fix-fn]
  (let [files&xmls (parse-xmls annons-dir)
        files-with-errors (->> files&xmls
                               (filter #(= nil (second %)))
                               (map first)
                               (map #(.getCanonicalPath %))
                               (seq))]
    (if files-with-errors
      (log-error&throw "Corrupt xml files found" files-with-errors)
      (reduce
       (fn [[classes problems] [file xml]]
         (if-let [cls (seq (map #(if % % "nil") (extract-classes xml)))]
           [(into classes cls) (if (all-classes-are-fine? cls cls2id annon-fix-fn) problems (conj problems file))]
           [classes problems]))
       [#{} []]
       files&xmls))))

(defn extract-all-classes-recursively [dir cls2id annon-fix-fn]
  (loop [[file & files] (.listFiles dir) classes #{} problems []]
    (cond
      (not file)
      [classes problems]

      (and (.isDirectory file) (= "annotations" (.getName file)))
      (let [[file-classes file-problems] (extract-all-classes file cls2id annon-fix-fn)]
        (recur files (into classes file-classes) (concat problems file-problems)))

      (.isDirectory file)
      (let [[dir-classes dir-problems] (extract-all-classes-recursively file cls2id annon-fix-fn)]
        (recur files (into classes dir-classes) (concat problems dir-problems)))

      :OTHERWISE
      (recur files classes problems))))


(defn is-ds-dir? [dir]
  (let [annon-dir (io/file dir "annotations")
        with-jpg (some #(re-matches #".*\.jpg$" (.getName %)) (.listFiles dir))
        sub-dirs (count (filter #(.isDirectory %) (.listFiles dir)))
        with-annons (.exists annon-dir)
        is-leaf (or (= 0 sub-dirs) (and with-annons (= 1 sub-dirs)))
        it-is (and with-jpg is-leaf)]
    (if it-is
      {:jpg (count (filter #(re-matches #".*\.jpg$" (.getName %)) (.listFiles dir)))
       :xml (if with-annons (count (filter #(re-matches #".*\.xml$" (.getName %)) (.listFiles annon-dir))) 0)})))

(defn create-ds-set [root-dir excluding-set ds-set total-jpg total-xml]
 (loop [[sub-dir & sub-dirs] (filter #(.isDirectory %) (.listFiles root-dir))
        ds-set ds-set
        total-jpg total-jpg
        total-xml total-xml]
   (if sub-dir
     (let [ds-dir-info (is-ds-dir? sub-dir)]
       (cond
         ds-dir-info
         (if (or (excluding-set (.getName sub-dir)) (ds-set (.getName sub-dir)))
           (log-error&throw (format "Directorios dentro del dataset deben ser únicos, problema con '%s'" (.getCanonicalPath sub-dir)))
           (recur sub-dirs
                  (assoc ds-set (.getName sub-dir) ds-dir-info)
                  (+ total-jpg (:jpg ds-dir-info))
                  (+ total-xml (:xml ds-dir-info))))

         (.isDirectory sub-dir)
         (let [[ds-set total-jpg total-xml] (create-ds-set sub-dir excluding-set ds-set total-jpg total-xml)]
           (recur sub-dirs ds-set total-jpg total-xml))

         :OTHERWISE
         (recur sub-dirs ds-set total-jpg total-xml)))
     [ds-set total-jpg total-xml])))

(defn create-ds-info [root-dir]
  (let [train-dir (io/file root-dir "train")
        val-dir (io/file root-dir "val")
        [train-set train-jpg train-xml] (create-ds-set train-dir {} {} 0 0)
        [val-set val-jpg val-xml] (create-ds-set val-dir train-set {} 0 0)]
    (let [result {:train (into (sorted-map) train-set)
                  :train-total {:images train-jpg :xml train-xml}
                  :val (into (sorted-map) val-set)
                  :val-total {:images val-jpg :xml val-xml}}]
      result)))

(defn print-ds-info [ds-info]
  (pp/pprint ds-info)
  ds-info)

(defn read&create-fix-fn [annon-fix-file]
  (if (and annon-fix-file (.exists (io/file annon-fix-file)))
    (let [annon-fixes (read-string (slurp annon-fix-file))
          fixes (reduce (fn [result [re fix]]
                          (conj result [(re-pattern re) fix]))
                        []
                        annon-fixes)]
      (if (seq fixes)
        (fn fix-name [name]
          (reduce (fn [result [re fix]]
                    (if (re-matches re name)
                      (reduced fix)
                      name))
                  name
                  fixes))
        identity))
    identity))

(defn all-classes-are-fine? [all-classes cls-id-edn class-fixer]
  (every? #(cls-id-edn %) (map class-fixer all-classes)))
