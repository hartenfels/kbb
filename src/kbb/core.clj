(ns kbb.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :as cli])
  (:import [java.nio.file LinkOption Files Path Paths]
           [java.util.regex Pattern])
  (:gen-class))


(defn die!
  ([message] (die! "%s" message))
  ([fmt arg & args]
   (throw (RuntimeException. (apply format (concat [fmt arg] args))))))

(defn empty-array [class]
  (into-array class []))

(defn stream-seq [stream]
  (-> stream .iterator iterator-seq))


(defn path [path-or-string & more]
  (if (instance? Path path-or-string)
    (if (empty? more)
      path-or-string
      (reduce #(.resolve %1 %2) path-or-string more))
    (apply path (cons (Paths/get path-or-string (empty-array String)) more))))

(defmacro is-x? [predicate path-or-string]
  `(~predicate (path ~path-or-string) (empty-array LinkOption)))

(defn is-directory? [path-or-string]
  (is-x? Files/isDirectory path-or-string))

(defn is-file? [path-or-string]
  (is-x? Files/isRegularFile path-or-string))

(defn basename [path-or-string]
  (str (.getFileName (path path-or-string))))

(defn ls [dir]
  (stream-seq (Files/list (path dir))))

(defn read-first-non-blank-line [path-or-string]
  (with-open [reader (Files/newBufferedReader (path path-or-string))]
    (->> reader
         line-seq
         (filter (complement string/blank?))
         first)))

(defn slurp-path [path-or-string]
  (-> path-or-string path str slurp))


(defn- strip-leading-digits [column]
  (string/replace-first column #"\A[0-9]+" ""))

(defn column-title [column]
  (-> column basename strip-leading-digits string/trim))

(defn task-title [task]
  (or (read-first-non-blank-line task)
      (-> task basename string/trim)))

(defn task-content [task]
  (slurp-path task))


(defn- list-with-predicate [dir predicate]
  (->> dir
       ls
       (filter predicate)
       (sort-by basename)))

(defn list-columns [dir]
  (list-with-predicate dir #(and (is-directory? %)
                                 (re-find #"\A[0-9]+.*\S" (basename %)))))

(defn list-tasks-in-column [column]
  (list-with-predicate column is-file?))


(defn to-column-and-task-map [column]
  {:column column
   :tasks (into [] (list-tasks-in-column column))})

(defn read-all-tasks-by-columns [dir]
  (->> dir
       list-columns
       (map to-column-and-task-map)))

(defn flatten-by-column [{:keys [column tasks]}]
  (map #(array-map :column column :task %) tasks))

(defn read-all-tasks-flat [dir]
  (->> dir
       read-all-tasks-by-columns
       (mapcat flatten-by-column)))

(defn read-filtered-tasks-flat [predicate dir]
  (->> dir
       read-all-tasks-flat
       (filter predicate)))

(defn read-title-matched-tasks-flat [string-or-pattern dir]
  (let [regex (if (instance? Pattern string-or-pattern)
                string-or-pattern
                (re-pattern (str "(?i)" string-or-pattern)))]
    (read-filtered-tasks-flat #(re-find regex (-> % :task task-title)) dir)))


(defn- tasks-titles-by-column-title [{:keys [column tasks]}]
  {:column (column-title column)
   :tasks (map task-title tasks)})

(defn all-task-titles-by-column-titles [dir]
  (map tasks-titles-by-column-title (read-all-tasks-by-columns dir)))



(defn format-tasks-by-column [{:keys [column tasks]}]
  (concat [column]
          [(string/replace column #"." "=")]
          [""]
          (map (partial str "* ") tasks)
          (if (empty? tasks) [] [""])))

(defn format-all-tasks-by-columns [dir]
  (string/join "\n" (->> dir
                         all-task-titles-by-column-titles
                         (mapcat format-tasks-by-column))))

(defn cmd-board [dir args]
  (if (empty? args)
    (format-all-tasks-by-columns dir)
    (die! "Unknown arguments to 'board' command: %s" args)))


(defn format-full-task [{:keys [column task]}]
  (format "(%s) %s" (column-title column) (task-content task)))

(defn show-single-task-by-pattern [pattern dir]
  (let [tasks (read-title-matched-tasks-flat pattern dir)]
    (case (count tasks)
      0 (die! "No candidates found for /%s/" pattern)
      1 (-> tasks first format-full-task)
      (die! "Found multiple candidates for /%s/" pattern))))

(defn cmd-show [dir args]
  (show-single-task-by-pattern (string/join " " args) dir))


(defn run-command! [[cmd & args] {:keys [board-dir]}]
  (condp contains? cmd
    #{"b" "board"} (cmd-board board-dir args)
    #{"s" "show"} (cmd-show board-dir args)
    (die! "Unknown command: '%s'" cmd)))

(defn get-env [arg]
  (let [vector-arg? (vector? arg)
        key (if vector-arg? (first arg) arg)
        append-path (if vector-arg? (rest arg) nil)
        value (System/getenv key)]
    (if (and value append-path)
      (str (apply path (cons value append-path)))
      value)))

(defn get-first-env [& args]
  (->> args
       (map get-env)
       (filter (complement nil?))
       (filter (complement string/blank?))
       first))

(def cli-options
  [["-b" "--board-dir PATH" (str "Directory where the board is found in. "
                                 "Defaults to environment variable KBB_ROOT "
                                 "or ~/.kbb-board if that is unset.")
    :default-fn (fn [_] (or (get-first-env "KBB_ROOT" ["HOME" ".kbb-board"])
                            (die! "No board directory found, use -b")))]])

(defn main! [& args]
  (let [{:keys [options arguments errors]} (cli/parse-opts args cli-options
                                                           :in-order true)]
    (if errors
      (die! "Error handling program arguments:\n%s" (string/join "\n" errors))
      (run-command! (if (empty? arguments) ["board"] arguments) options))))

(defn -main [& args]
  (try
    (println (str "\n" (apply main! args)))
    (catch Exception e
      (binding [*out* *err*]
        (println (format "\nError: %s\n" (.getMessage e))))
      (System/exit 1))))
