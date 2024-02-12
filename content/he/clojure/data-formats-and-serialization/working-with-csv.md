---
title:                "עובדים עם CSV"
date:                  2024-02-03T19:19:29.519306-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (ערכים מופרדים בפסיק) כוללת פענוח ויצירת נתוני טקסט מובנים כשורות ועמודות, דומה לנתוני גיליון אלקטרוני. תהליך זה הוא חיוני להחלפת נתונים בין יישומים, מסדי נתונים, ולמשימות של טרנספורמציה של נתונים, בשל הקבלה הרחבה של CSV כפורמט קל משקל ואינטרופרבילי.

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
