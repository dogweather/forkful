---
title:                "עבודה עם קבצי CSV"
html_title:           "Clojure: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## למה
למה לעסוק בעבודה עם קבצי CSV בעזרת קלוז'ור? פשוט מכיוון שקבצי CSV הם פורמט נפוץ לאחסון והעברת נתונים ועבודה עם קבצי CSV בעזרת קלוז'ור מספקת דרך יעילה ופשוטה לטפל בנתונים.

## איך לעשות זאת
כדי לעבד קבצי CSV בקלוז'ור נצטרך להתאים את הקוד להתאמה לפורמט הנתונים המיועד לשימוש. קוד הדוגמה הבא ממחיש איך לקרוא קובץ CSV ולהציג את הנתונים שבו:

```Clojure
(require '[clojure.java.io :as io])
(require '[clojure-csv.core :as csv])

(def csv-file (io/reader "my_file.csv")) ; קריאת קובץ CSV
(def data (csv/read-csv csv-file)) ; קריאת הנתונים מתוך הקובץ
(println data) ; הדפסת הנתונים שבקובץ

; פלט:
; ([שורה 1 תא 1] [שורה 1 תא 2] [שורה 1 תא 3])
; ([שורה 2 תא 1] [שורה 2 תא 2] [שורה 2 תא 3])
```

ניתן גם לעשות שימוש בספריית `clojure.data.csv` כדי לכתוב נתונים לקובץ CSV. הדוגמה הבאה מראה איך ליצור קובץ CSV חדש ולכתוב נתונים אליו:

```Clojure
(require '[clojure.data.csv :as csv])

(def new-file (csv/writer "new_file.csv" :end-of-line "\n")) ; יצירת קובץ CSV חדש

(def data [["שורה 1 תא 1" "שורה 1 תא 2" "שורה 1 תא 3"] ; הגדרת הנתונים שישמרו בקובץ
           ["שורה 2 תא 1" "שורה 2 תא 2" "שורה 2 תא 3"]])

(dotimes [i (count data)] ; לולאת טיפול בנתונים
  (.write new-file (nth data i)))

(.close new-file) ; סגירת הקובץ

; פלט (קובץ new_file.csv):
; שורה 1 תא 1,שורה 1 תא 2,שורה 1 תא 3
; שורה 2 תא 1,שורה 2 תא 2,שורה 2 תא 3
```

## העיון העמוק
עכשיו שאנו מכירים את הסיסמאות ה