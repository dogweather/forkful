---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:29.519306-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DC-Clojure \u05D0\u05D9\u05DF \u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D9\u05EA \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 CSV \u05DE\
  \u05D5\u05D1\u05E0\u05D9\u05EA \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\
  \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8 \u05E9\u05DC\u05D4, \u05D0\u05DA \u05E0\u05D9\
  \u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05EA `clojure.data.csv` \u05DC\u05DE\u05D8\u05E8\u05D4 \u05D6\u05D5. \u05E8\
  \u05D0\u05E9\u05D9\u05EA, \u05D4\u05D5\u05E1\u05E3 \u05D0\u05EA \u05D4\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 \u05DC\u05EA\u05DC\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.741014-06:00'
model: gpt-4-0125-preview
summary: "\u05DC-Clojure \u05D0\u05D9\u05DF \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D9\
  \u05EA \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 CSV \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\
  \u05D8 \u05E9\u05DC\u05D4, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `clojure.data.csv`\
  \ \u05DC\u05DE\u05D8\u05E8\u05D4 \u05D6\u05D5."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

## איך ל:


### קריאת קובץ CSV
ל-Clojure אין פונקציית פיענוח CSV מובנית בספריית הסטנדרט שלה, אך ניתן להשתמש בספריית `clojure.data.csv` למטרה זו. ראשית, הוסף את הספרייה לתלות הפרויקט שלך.

ב-`project.clj` שלך, הוסף את התלות הבאה:
```clojure
[clojure.data.csv "1.0.0"]
```
לקריאת קובץ CSV והדפסת כל שורה:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "path/to/yourfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
זה יפלט כל שורה מה-CSV כווקטור של Clojure.

### כתיבה לקובץ CSV
לכתיבת נתונים לקובץ CSV, ניתן להשתמש באותה ספריית `clojure.data.csv`:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "path/to/outputfile.csv")]
    (csv/write-csv writer data)))
```
זה יוצר או דורס את `outputfile.csv`, ממלא אותו בנתונים המפורטים.

### שימוש בספריית צד שלישי: `clojure.data.csv`
למרות ש-`clojure.data.csv` הוא, ללא ספק, הספרייה הפשטנית ביותר לטיפול ב-CSV ב-Clojure, למשימות מורכבות יותר, כמו התמודדות עם CSV-ים עם תווים מיוחדים או מפרידים לא רגילים, עשויים להיות שימושיים אפשרויות נוספות באקוסיסטם או אף לשקול חיבור עם ספריות של Java כמו Apache Commons CSV. עם זאת, לרוב משימות העיבוד של CSV ב-Clojure, `clojure.data.csv` מספק ערכה פשוטה ויעילה של כלים.
