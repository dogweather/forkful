---
title:    "Clojure: שיאת תת מחרוזות"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

למיהוי בפעולת ייצוא החריגים רק בתוך 1-2 משפטים.


## איך לעשות

תוך שימוש בדוגמאות קוד והפלט השאלתי בתוך קוד דוגמה "```Clojure ...```".

 ```Clojure
 ; דוגמה 1: מיהוי התו הראשון מתוך מחרוזת
 (defn extract-first-char [string]
   (subs string 0 1))
 
 (extract-first-char "שלום") ; יחזיר "ש"

; דוגמה 2: מיהוי תוים מתוך מחרוזת על פי טווח תוים
(defn extract-range [string start end]
   (subs string start end))
 
 (extract-range "שלום" 1 3) ; יחזיר "לו"
 ```

## העמקה

לימיהוי תת מחרוזות, אנו משתמשים בפונקציית subs. פונקצית subs מקבלת כפרמטרים מחרוזת, נקודת התחלה וסוף ומחזירה את החלק הרלוונטי של המחרוזת. ניתן לכתוב את נקודת ההתחלה והסוף ע״י מטבעי המספיק של התוים בתחילת המחרוזת. לדוגמה, נקודת ההתחלה של המחרוזת "שלום" היא 0, הנקודת סוף היא 2, כיוון שלום מורכב מ-3 תוים.

## ראה גם

- תיעוד רשמי של subs: https://clojuredocs.org/clojure.core/subs
- מדריך לפונקציות מחרוזות בקלוז'ור: https://www.clojure.org/guides/learn/functions_strings
- פרוייקט הקלוז'ור הרשמי: https://clojure.org/