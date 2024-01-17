---
title:                "עובדים עם קבצי CSV"
html_title:           "Clojure: עובדים עם קבצי CSV"
simple_title:         "עובדים עם קבצי CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

# מה זה ולמה?

עבודה עם CSV היא דרך נפוצה לעיבוד וניתוח נתונים בפורמט טקסטואלי פשוט. תוכניות תוכנה משתמשות בפורמט זה כדי לשמור נתונים בטבלאות מבניות, מה שהופך את התעבודה עם CSV לחיונית לתכנותנים.

# איך לעשות:

הנה כמה דוגמאות של קוד ב-Clojure שמציגות איך לעבוד עם CSV ומה הפלט הצפוי של כל קוד.

```Clojure
;; ייבוא חבילת CSV
(:require [clojure.data.csv :as csv])

;; קריאת קובץ CSV והדפסת הנתונים
(with-open [fcsv (clojure.java.io/reader "data.csv")]
  (doseq [row (csv/read-csv fcsv)]
    (println row)))                                 ;; פלט: ["שם", "גיל", "מין"], ["דנה", "28", "נקבה"]

;; יצירת קובץ CSV חדש וכתיבת נתונים לתוכו
(with-open [fcsv (clojure.java.io/writer "new_data.csv")]
  (csv/write-csv fcsv [["שם", "גיל", "מין"], ["יוסי", "42", "זכר"]]))   ;; פלט: שם,גיל,מין,יוסי,42,זכר
```

# מעמקים:

היסטורית הפורמט CSV החלה בשנות ה-1970 והחלה לשתף פעולה רבה עם ספריות לשפות תכנות שונות. אם אתם מחפשים אלטרנטיבות לעבודה עם CSV ב-Clojure, ניתן להשתמש בחבילות כמו data.csv ווביב, שמציגות פתרונות יעילים יותר לאינטראקציה עם קבצי CSV.

עוד דברים שכדאי לדעת על פורמט CSV בקשר לעבודה עם Clojure:

- פורמט CSV אינו מגדיר סטנדרט לנתונים, ולכן יתכנו דרכים שונות להתאימות לשמירה וקריאה של נתונים מסוימים.
- אין כיוון קבע לטבלאות בפורמט CSV, ולכן ייתכן שייתכנו שגיאות בטבלאות עם מבנה לא מסוים.

# ראו גם:

בקישורים המצורפים תוכלו למצוא ספריות נוספות לעיבוד נתוני CSV ב-Clojure ומדריכים נוספים לעבודה עם פורמט זה.

- [Data CSV](https://github.com/clojure/data.csv)
- [Webbiv](https://github.com/yogthos/webbiv)
- [איך לעבוד עם CSV ב-Clojure](https://elbenshira.medium.com/how-to-work-with-csv-in-clojure-901860d8c0a8)