---
date: 2024-01-20 17:54:44.108453-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1\u05E7\u05DC\u05D5\u05D6'\u05E8 (Clojure) \u05D4\u05D9\u05D0\
  \ \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D4\u05D1\u05D9\u05D0 \u05D0\u05EA \u05D4\u05D8\
  \u05E7\u05E1\u05D8 \u05DE\u05D4\u05D3\u05D9\u05E1\u05E7 \u05DC\u05D6\u05D9\u05DB\
  \u05E8\u05D5\u05DF. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\
  \u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D4\
  \u05D2\u05D3\u05E8\u05D5\u05EA \u05D5\u05DC\u05D8\u05E2\u05D5\u05DF \u05E1\u05E7\
  \u05E8\u05D9\u05E4\u05D8\u05D9\u05DD."
lastmod: '2024-03-13T22:44:38.733175-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1\u05E7\u05DC\u05D5\u05D6'\u05E8 (Clojure) \u05D4\u05D9\u05D0 \u05E4\
  \u05E9\u05D5\u05D8 \u05DC\u05D4\u05D1\u05D9\u05D0 \u05D0\u05EA \u05D4\u05D8\u05E7\
  \u05E1\u05D8 \u05DE\u05D4\u05D3\u05D9\u05E1\u05E7 \u05DC\u05D6\u05D9\u05DB\u05E8\
  \u05D5\u05DF. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D4\u05D2\
  \u05D3\u05E8\u05D5\u05EA \u05D5\u05DC\u05D8\u05E2\u05D5\u05DF \u05E1\u05E7\u05E8\
  \u05D9\u05E4\u05D8\u05D9\u05DD."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
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
