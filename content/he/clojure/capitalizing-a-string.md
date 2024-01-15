---
title:                "קיפוטיליזציה של מחרוזת"
html_title:           "Clojure: קיפוטיליזציה של מחרוזת"
simple_title:         "קיפוטיליזציה של מחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##למה
 כתיבת קוד בפורמט Clojure כולל השתמשות בפונקציות המטרה שלהן היא לסייע בטיפול ועיבוד נתונים. כאחד מפורמטי התכנות המודרניים, Clojure מציע כלים נוחים עבור מתכנתים לפתור בעיות בדרך יעילה וקצרה.

##איך לעשות
בקוד שלהלן נדגים איך לבצע קפיטליזציה (שינוי התווים הראשונים לאותיות גדולות) של מחרוזת ב-Clojure:
``` Clojure
(defn capitalize-string [s]
  (clojure.string/upper-case-first s))

(capitalize-string "hello world")

; Output: "Hello world"
```
ניתן לראות שהפונקציה מקבלת מחרוזת כארגומנט ומחזירה את אותה המחרוזת עם האות הראשונה נעלמת וה-W מוחלפת באות גדולה. פונקציית הקפיטליזציה הזו מובנת מראש וניתנת לשימוש בפורמט Clojure.

##עיון מעמיק
אותו תוכן ניתן לקרוא בעזרת הפונקציה `name` המקבלת משתנה ומחזירה את שם המשתנה בפורמט :keyword (=: בברירת מחדל).
לדוגמה, אם נרצה לקפיטליזציה של המחרוזת "hello world" על ידי שימוש בפונקציית `name`, הקוד יראה כך:
```Clojure
(defn capitalize-string [s]
  (let [first-char (name (first s))
        rest-chars (subs s 1)]
    (str/upper-case first-char rest-chars)))

(capitalize-string "hello world")

; Output: "Hello world"
```
כפי שאנו רואים, ניתן לעשות עיבודים נוספים על מחרוזת נתונה ע"י שימוש בפונקציות שונות עםב-Pure Clojure.

##ראה גם
- [קורס חינמי של Clojure](https://www.clojure.org)
- [Clojure: מדריך למתכנתים](https://clojure.org/guides/getting_started)
- [שפת Clojure](https://en.wikipedia.org/wiki/Clojure)