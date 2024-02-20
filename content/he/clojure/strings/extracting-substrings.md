---
date: 2024-01-20 17:45:21.160180-07:00
description: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05D6\u05D4 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4\
  \ \u05D0\u05EA\u05D4 \u05DC\u05D5\u05E7\u05D7 \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA. \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3\
  \ \u05D5\u05DC\u05E0\u05EA\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05DE\
  \u05E9\u05DC, \u05DC\u05D4\u05D5\u05E6\u05D9\u05D0 \u05DE\u05D9\u05D3\u05E2 \u05E1\
  \u05E4\u05E6\u05D9\u05E4\u05D9 \u05DE\u05D8\u05E7\u05E1\u05D8."
lastmod: 2024-02-19 22:04:57.956636
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D6\u05D4 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\
  \u05EA\u05D4 \u05DC\u05D5\u05E7\u05D7 \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA. \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05D5\
  \u05DC\u05E0\u05EA\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05DE\u05E9\
  \u05DC, \u05DC\u05D4\u05D5\u05E6\u05D9\u05D0 \u05DE\u05D9\u05D3\u05E2 \u05E1\u05E4\
  \u05E6\u05D9\u05E4\u05D9 \u05DE\u05D8\u05E7\u05E1\u05D8."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות זה פעולה שבה אתה לוקח חלק ממחרוזת. תכנותים עושים זאת כדי לעבד ולנתח נתונים, למשל, להוציא מידע ספציפי מטקסט.

## איך לעשות:
ב-Clojure אפשר לחלץ תת-מחרוזת בקלות עם הפונקציה `subs`. דוגמא:

```clojure
(let [text "שלום עולם"]
  (println (subs text 0 4))) ; ידפיס "שלום"

(let [data "12345-67890"]
  (println (subs data 6)))  ; ידפיס "67890"
```

פלט:

```
שלום
67890
```

## טבילה עמוקה
תחילת השימוש בפונקציית `subs` ב-Clojure מתאריכה לראשית השפה, ומבוססת על פונקציות חילוץ מחרוזות משפות תכנות אחרות. דבר חשוב לזכור הוא שמדובר בפונקציה טהורה (pure function), אשר נותנת תוצאה קבועה לקלטים קבועים ואינה גורמת לתופעות לוואי. לחלופין, ישנם כלים אחרים ב-Clojure לעבודה עם טקסט כמו regex (ביטויים רגולריים), אם יש צורך במניפולציה מורכבת יותר.

## ראו גם
- התיעוד הרשמי של הפונקציה `subs`: https://clojuredocs.org/clojure.core/subs
- מדריך לביטויים רגולריים ב-Clojure: https://www.braveclojure.com/regex/
- פוסט בבלוג על עבודה עם טקסט ב-Clojure: https://lispcast.com/clojure-strings/
