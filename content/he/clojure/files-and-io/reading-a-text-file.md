---
title:                "קריאת קובץ טקסט"
aliases:
- /he/clojure/reading-a-text-file.md
date:                  2024-01-20T17:54:44.108453-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט בקלוז'ר (Clojure) היא פשוט להביא את הטקסט מהדיסק לזיכרון. תכניתנים עושים את זה כדי לעבד נתונים, לקרוא הגדרות ולטעון סקריפטים.

## איך לעשות:
כדי לקרוא קובץ טקסט בקלוז'ר, תשתמשו בפונקציה `slurp`. זה קוד פשוט לקריאת פנימה:

```Clojure
;; קריאת קובץ טקסט למחרוזת
(let [content (slurp "path/to/your/file.txt")]
  (println content))

;; דוגמת פלט
; הנה הטקסט מהקובץ שלך!
```

או אם אתם צריכים לקרוא קובץ קטן כל שורה בנפרד:

```Clojure
;; קריאת קובץ טקסט לרשימת שורות
(with-open [rdr (reader "path/to/your/file.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))

;; דוגמת פלט
; שורה 1
; שורה 2
; שורה 3
```

## עיון מעמיק
קריאת קבצים היא עניין ישן כמעט כמו המחשבים עצמם. `slurp` זה קל ונוח, אבל במקרים של קבצים גדולים, ייתכן שתרצו לקרוא בזרימה (`stream`) ולא הכל במכה אחת. הפונקציה `line-seq` שנראיתה מעלה עובדת עם `reader`, שכן היא פולטת ערוך זרימתי של שורות, מותירה זמן לעיבוד כל שורה לפני קריאת הבאה.

בנוסף, בקלוז'ר יש עוד אופציות, כמו פונקציות `read-line`, `read-lines`, או שימוש ב`java.io` ישירות לשליטה מעמיקה יותר.
ייתכנו גם ספריות שלישיות שיכולות להוסיף יכולות מעמיקות נוספות או לייעל את קריאת קבצים במקרים מסוימים.

## ראו גם
התיעוד הרשמי של Clojure על קלט/פלט:
- [Clojure - java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)

מדריכים ומאמרים שיכולים לעזור:
- [ClojureDocs - slurp](https://clojuredocs.org/clojure.core/slurp)

ואם אתם רוצים ללמוד יותר על עיבוד טקסט בקלוז'ר:
