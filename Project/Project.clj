(ns Project)

(def basic-regex #"(?x)
    ( \"[^\n\"]*\" )                                                                                                  # Group 1: Strings
  | ( REM.* )                                                                                                         # Group 2: Commment
  | ( \b(?:DATA|DEF|DIM|END|FOR|TO|STEP|GOSUB|GOTO|IF|THEN|ELSE|INPUT|LET|NEXT|ON|PRINT|READ|RESTORE|RETURN|STOP)\b ) # Group 3: Reserved
  | ( \b(?:ABS|ASC|ATN|CHR\$|COS|EXP|INT|LEFT\$|LEN|LOG|MID\$|RND|RIGHT\$|SGN|SIN|SQR|STR\$|TAB|TAN|VAL)\b )          # Group 4: Function
  | ( \b[A-Za-z]{1,2}\d?\$?\b )                                                                                       # Group 5: Variable
  | ( \b\d+(?:\.\d+)?(?:[eE][+-]?\d+)?\b )                                                                            # Group 6: Number
  | ( <>|<=|>=|<|>|=|:|\+|-|\*|/|\^|\(|\)|,|; )                                                                       # Group 7: Punctuation
  | ( \s+ )                                                                                                           # Group 8: Spaces
  | ( . )                                                                                                             # Group 9: Ivalid
")

(def categories [nil :string :comment :reserved :function :variable
                 :number :punctuation :spaces :invalid-character])

(defn capturing-group-index
  [v]
  (inc (count (take-while nil? (rest v)))))

(defn lexical-analysis
  [file-content]
  (let [matches (re-seq basic-regex file-content)]
      (map (fn [match]
             [(match 0) (categories (capturing-group-index match))])
           matches)))

(defn htmlize
  [s]
  (map (fn [[value category]]
         (format "<span class=\"%s\">%s</span>"
                 (symbol category)
                 value))
       s))

(def html-template (slurp "project_work_files/template_project.html"))

(defn text->html
  [in-name out-name]
  (let [file-content (slurp in-name)]
    (spit out-name
          (format html-template
                  file-content
                  (apply str (htmlize (lexical-analysis file-content)))))))

(text->html "project_work_files/bcg/amazing.bas" "project_work_files/result_project.html")
