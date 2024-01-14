---
title:                "Clojure: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה?

יצירת קובץ זמני נדרשת כאשר יש צורך להשתמש בקובץ זמני בזמן ריצת התוכנית שלנו. לדוגמה, כאשר יש צורך לקריאת או כתיבת נתונים לקובץ זמני, או כאשר נדרשת כתיבה ליישומי הזכרון של המחשב.

## איך לעשות זאת?

כדי ליצור קובץ זמני בלוןטית, אנו נשתמש בפונקציה "with-open" עם "java.io.File". הפונקציה תיצור ותסגור את הקובץ אוטומטית בסוף ריצת הפונקציה.

```Clojure
(with-open [temp-file (java.io.File/createTempFile "file" ".txt")]
    ;; כאן ניתן לבצע פעולות על הקובץ הזמני
    ;; למשל קריאה/כתיבה לקובץ
    (println "הקובץ הזמני נוצר בהצלחה")
))
```

תוצאה:

```bash
הקובץ הזמני נוצר בהצלחה
```

## מעמקים

כאשר אנו יוצרים קובץ זמני, תחת הקרקע הוא מיוצר תיקייה זמנית במחיצת התקן. את השם של התיקייה הזאת אנו יכולים לקבוע באמצעות הפרמטר הראשון של הפונקציה "java.io.File/createTempFile". אם נשתמש בפרמטר "nil", תיחשב לנו תיקייה זמנית במיקום התיקייה הזמנית המטריצה של המערכת הפעילה.

ניתן גם להוסיף פרמטר נוסף לפונקציה "with-open" שקובע את תכולת הקובץ הזמני עבורנו. בכך יתאפשר לנו להשתמש בקובץ זמני כקובץ תבנית.

כעת אנו יכולים ליצור קובץ זמני ולמלאו עם הנתונים הרצויים באמצעות הפרמטר השני של הפונקציה "with-open" כך:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "file" ".txt")
            writer (java.io.FileWriter. temp-file)]
    (.write writer "שלום