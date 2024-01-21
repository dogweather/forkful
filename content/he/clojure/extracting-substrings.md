---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:45:21.160180-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/extracting-substrings.md"
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