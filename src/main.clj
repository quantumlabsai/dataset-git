(ns main
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

(def cli-options
  ;; An option with a required argument
  [["-H" "--help"]
   ["-a" "--annon-fix fixes.edn" "Nombre del archivo edn con vector de parejas 'regexp' -> 'clase' para corregir anotaciones"
    :validate [(fn [arg]
                 (let [edn (io/file arg)]
                   (and (.exists edn) (.isFile edn)))) "Archivo edn debe existir"]]
   ["-d" "--dropping-classes classes.edn" "Nombre del archivo edn con vector de clases a no conservar"
    :validate [(fn [arg]
                 (let [edn (io/file arg)]
                   (and (.exists edn) (.isFile edn)))) "Archivo edn debe existir"]]
   ["-b" "--background-percent 0-100" "Porcentaje de imagenes que solo tienen backgrownd a copiar"
    :parse-fn #(Integer/parseInt %)
    :validate [(fn [arg]
                 (and (>= arg 0) (<= arg 100))) "Porcentaje debe ser entero entre 0-100"]
    :default 100]
   ["-c" "--conf <file>" "Nombre del archivo edn de configutación 'base'"
    :validate [(fn [arg]
                 (let [conf (io/file arg)]
                   (.exists conf))) (format "Archivo no existe!")]
    :assoc-fn (fn [m k v]
                (merge m (-> (slurp v) (read-string))))]
   ["-E" "--pre-commit" "Run only pre-commit validation"]
   ["-O" "--post-commit" "Run only post-commit, generat dataset"]
   ["-e" "--ext <str>" "Extensiones validas ej: 'jpg' 'gif' 'png' en las imagenes"
    :validate [(fn [arg]
                 (let [s (S/replace arg " " "")]
                   (and
                    (= arg s)))) "Ext no debe contener espacios"]
    :default []
    :assoc-fn (fn [m k v]
                (update m k #(conj % v)))]
   ["-f" "--force" "Borra y recrea el directorio de salida sin preguntar !!!"]
   ["-h" "--height <nnn>" "Alto en pixeles de la imagen salida"
    :parse-fn #(Integer/parseInt %)
    :validate [(fn [arg]
                 (and (<= arg 3000) (>= arg 0))) "Height debe ser un numero entero"]]
   ["-i" "--in dir" "Input images directory"
    :validate [(fn [arg]
                 (let [dir (io/file arg)]
                   (and (.exists dir) (.isDirectory dir)))) "Directorio debe existir"]]
   ["-l" "--left 0-100 (-1)" "Posible porcentaje a perder por la izquierda de la imagen de entrada (-1 para desactivar clipping)"
    :parse-fn #(Integer/parseInt %)
    :validate [(fn [arg]
                 (or (= arg -1)
                     (and (>= arg 0) (<= arg 100)))) "Porcentaje debe ser entero entre 0-100"]]
   ["-m" "--cls2id cls2id.edn" "Nombre del archivo edn con el mapa de class a id deseado"
    :validate [(fn [arg]
                 (let [edn (io/file arg)]
                   (and (.exists edn) (.isFile edn)))) "Archivo edn debe existir"]]
   ["-o" "--out dir" "Output images directory"
    :validate [(fn [arg]
                 (let [dir (io/file arg)]
                   (or (not (.exists dir))
                       (.isDirectory dir)))) "Directorio no debe existir o ser un directorio"]]
   ["-t" "--top 0-100 (-1)" "Posible porcentaje a perder por arriba de la imagen de entrada (-1 para desactivar clipping)"
    :parse-fn #(Integer/parseInt %)
    :validate [(fn [arg]
                 (or (= arg -1)
                     (and (>= arg 0) (<= arg 100)))) "Porcentaje debe ser entero entre 0-100"]]
   ["-w" "--width <nnn>" "Ancho en pixeles de la imagen salida"
    :parse-fn #(Integer/parseInt %)
    :validate [(fn [arg]
                 (and (<= arg 3000) (>= arg 0))) "Width debe ser un numero entero"]]
   ["-x" "--with-xml" "Si se pone esta opción se copian y reajustan las anotaciones xml"]
   ["-r" "--run" "Si se pone esta opción ejecuta la creacion del ds si es false solo valida la estructura"
    :default false]

   ["-p" "--prefix datasource-prefix" "Prefijo para el nombre del dataset generado"]
   ])


(defn -main [& args]
  (println "Iniciando proceso de limpieza!")
  (try
    (let [{:keys [options arguments errors summary] :as m} (parse-opts args cli-options)]
      (when (seq errors)
        (doseq [err errors]
          (println err))
        (System/exit 1))
      (let [{:keys [ext in out width height top left cls2id dropping-classes]} options]
        (println "Options:")
        (pp/pprint (into (sorted-map) options))
        (if (:help options)
          (println (:summary m))
          (cond
            (:pre-commit options)
            (core/pre-commit options)

            (:post-commit options)
            (core/post-commit options)

            :OTHERWISE
            (throw (Exception. "Enable --pre-commit or --post-commit"))

            ))))
  (catch Throwable e
    (println "Process terminated with error " e)
    (System/exit 1))))
