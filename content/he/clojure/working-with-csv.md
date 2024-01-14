---
title:                "Clojure: עבודה עם csv"
simple_title:         "עבודה עם csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

בלוג למתכנתים שעוסקים בשפת Clojure: למה לעסוק בעבודה עם קבצי CSV?

## למה:

עם הקלטת המידע והמידע הנוסף הנמצא כיום בתוך תבניות נתונים, עבודה עם קבצי CSV נעשית כל כך נפוצה. בניגוד לפורמטים אחרים, קבצי CSV ניתן לקריאה ולכתיבה בקלות וניתן לעבדם בכלים שונים על מנת ליצור מדויקות ויצירתיות.

## כיצד לעבוד עם קבצי CSV:

בשפת Clojure, יש אפשרות לפעול עם קבצי CSV בעזרת הספרייה "clojure.csv". הנה דוגמא לאיך ניתן לקרוא קובץ CSV, לעבד אותו וליצור מחרוזת המכילה את המידע:

```Clojure
;; ייבוא הספרייה
(require '[clojure.string :as str])
;; קריאת הקובץ CSV
(def data (csv/read-csv "example.csv"))
;; עיבוד המידע
(def processed-data (map #(str/join ", " %) data))
;; הדפסת הפלט
(println processed-data)
```

הפלט יהיה:

"Name, Age, Occupation, Gender"  
"John, 32, Engineer, Male"  
"Sarah, 28, Teacher, Female"  
"David, 42, Doctor, Male"

ניתן גם לכתוב לקובץ CSV בעזרת הפונקציה "write-csv":

```Clojure
;; ייבוא הספרייה
(require '[clojure.string :as str])
;; מידע לכתיבה לקובץ
(def data [["Name" "Age" "Occupation" "Gender"]
           ["John" "32" "Engineer" "Male"]
           ["Sarah" "28" "Teacher" "Female"]
           ["David" "42" "Doctor" "Male"]])
;; כתיבת המידע לקובץ CSV
(csv/write-csv "output.csv" data)
```

## נכנסים לעומק:

ישנם כלים נוספים שניתן להשתמש בהם עבור עיבוד וניתוח קבצי CSV בשפת Clojure, כגון "clojure-csv" ו-"data.csv". בנוסף, קיימת אפשרות להתמודד עם קבצי CSV בקלות בעזרת ספריות חיצוניות שמאפשרות תפעול על נתוני CSV בצורה מתקדמת יותר.

## ראו גם:

 - [התיעדות לספרי