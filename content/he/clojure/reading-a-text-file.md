---
title:                "קריאת קובץ טקסט"
html_title:           "Clojure: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

עבור לעברית למטה



## למה

למה לקרוא קובץ טקסט? בעזרת לועזית, קריאת קבצים טקסט מספקת דרך נוחה לקריאה וכתיבה של נתונים מהוקשים על ידי משתמשים אחרים או מכוניות. זה מאפשר יישומי תוכנה לקרוא ולהתכתב עם מסמכי טקסט, ולהמליץ על סעיפים לאחת הסיבות הבסיסיות של Clojure.

## איך לעשות

כעת כשאנחנו יודעים למה נרצה לקרוא קובץ טקסט, בואו נלמד איך לעשות זאת בעזרת Clojure. ראשית, נצטרך לטעון את הספריה של "java.io", כך שנוכל לעבוד עם קבצים. אז, באמצעות הפונקציה "with-open" נפתח את הקובץ וניתן לקרוא את התוכן שלו באמצעות הפונקציה "slurp". כדי לקרוא קובץ מסוים, נשתמש בגדר נתיב הקובץ ונפריד בין כל סעיף על ידי השתמשות בפונקציה "split-lines". הנה משלם קוד לדוגמה:

```Clojure
(ns my-clojure-project.core
  (:require [java.io :as io])) ;Improting the "java.io" library
(defn read-file [file-path]
  ; Opening the file and reading its content
  (with-open [file (io/reader file-path)]
    (slurp file))) ; Returning the content of the file
(defn split-file [file-content]
  ; Splitting the file into separate lines
  (clojure.string/split-lines file-content))
```

הנה כיצד נראים התוצאות כאשר נקרא ונפצה את התוכן של קובץ טקסט:

```Clojure
(my-clojure-project.core/read-file "my-text-file.txt")
;; Output:
; "Hello, this is my text file.
; I hope you find it useful!
; Thanks for reading."

(my-clojure-project.core/split-file (my-clojure-project.core/read-file "my-text-file.txt"))
;; Output:
; ["Hello, this is my text file."
; "I hope you find it useful!"
; "Thanks for reading."]
```

## העומק הראשון

אם אתה מתעניין במילות מפתח, שדרוגים או הרחבות ואתה מרגיש כאילו המידע המסופק למעלה אינו די מפרט, ישנם כ