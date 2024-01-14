---
title:                "Clojure: קריאת ארגומנטים של שורת פקודה"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

גישת השורת פקודה היא חלק חשוב ונפוץ בתכנות בשפת Clojure. קריאת פרמטרים שנמצאים בשורת הפקודה יכולה להיות אידיאלית להתאמות אישיות, חישובים מראש וכל מיני מטרות שונות. קריאת פרמטרים מצידה של המשתמש מאפשרת לנו לעבוד בדיוק עם המידע שציפינו לקבל, מה שמאפשר יכולות תכנות מתקדמות יותר ופתרונות מותאמים לצרכים שונים. בכתבה הזאת נביט לבסיסים לקריאת פרמטרים משורת פקודה ונראה דוגמאות שימושיות.

## כיצד לקרוא פרמטרים משורת פקודה

כדי לקרוא פרמטרים משורת פקודה בשפת Clojure, נשתמש בפונקציה `clojure.tools.cli` מחבילת `tools.cli`. פונקציה זו מאפשרת לנו להגדיר את תבניות הפרמטרים שנמצאים בשורת הפקודה ולקבל את הערכים שנמצאים בהתאם. לדוגמה:

```Clojure
(ns example.core
  (:require [clojure.tools.cli :as cli]))

(def cli-options
  [["-n" "--name NAME" "Your name" :required true]
   ["-a" "--age AGE" "Your age" :required true]])

(defn -main [& args]
  (let [[opts args banner]
        (cli/parse-opts args cli-options)]

    (println (str "Hello " (:name opts) "!")))
```

פונקציית `parse-opts` מחזירה מפתח עם שמות הפרמטרים כמפתחות והערכים שנמצאים עבורם כערכים. בדוגמה שלנו, נגדיר שני פרמטרים חובה: `-n` עבור שם ו `-a` עבור גיל. כאשר משתמשים בתכנית, יש לספק את הפרמטרים עם התווים המתאימים. לדוגמה: `lein run -n Mickey -a 25`.

## לחקור עמוק יותר

קריאת פרמטרים משורת פקודה היא נושא מעניין ומשתנה בשפת Clojure. בנוסף ל