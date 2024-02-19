---
aliases:
- /he/clojure/rounding-numbers/
date: 2024-01-26 03:43:56.220720-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D4\u05D5\u05D0 \u05D4\u05EA\u05D0\u05DE\u05EA \u05DE\u05E1\u05E4\u05E8 \u05DC\
  \u05E9\u05DC\u05DD \u05D4\u05E7\u05E8\u05D5\u05D1 \u05D1\u05D9\u05D5\u05EA\u05E8\
  , \u05D0\u05D5 \u05DC\u05D3\u05D9\u05D5\u05E7 \u05E2\u05E9\u05E8\u05D5\u05E0\u05D9\
  \ \u05DE\u05E1\u05D5\u05D9\u05DD. \u05D0\u05E0\u05D5 \u05DE\u05E2\u05D2\u05DC\u05D9\
  \u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E9\
  \u05D8 \u05D0\u05D5\u05EA\u05DD \u05DC\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05D3\
  \u05DD, \u05DC\u05D4\u05E4\u05D7\u05D9\u05EA \u05E2\u05D5\u05DE\u05E1 \u05D7\u05D9\
  \u05E9\u05D5\u05D1\u05D9, \u05D0\u05D5 \u05DC\u05E2\u05DE\u05D5\u05D3 \u05D1\u05D3\
  \u05E8\u05D9\u05E9\u05D5\u05EA\u2026"
lastmod: 2024-02-18 23:08:52.475531
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D4\
  \u05D5\u05D0 \u05D4\u05EA\u05D0\u05DE\u05EA \u05DE\u05E1\u05E4\u05E8 \u05DC\u05E9\
  \u05DC\u05DD \u05D4\u05E7\u05E8\u05D5\u05D1 \u05D1\u05D9\u05D5\u05EA\u05E8, \u05D0\
  \u05D5 \u05DC\u05D3\u05D9\u05D5\u05E7 \u05E2\u05E9\u05E8\u05D5\u05E0\u05D9 \u05DE\
  \u05E1\u05D5\u05D9\u05DD. \u05D0\u05E0\u05D5 \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD\
  \ \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8\
  \ \u05D0\u05D5\u05EA\u05DD \u05DC\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05D3\u05DD\
  , \u05DC\u05D4\u05E4\u05D7\u05D9\u05EA \u05E2\u05D5\u05DE\u05E1 \u05D7\u05D9\u05E9\
  \u05D5\u05D1\u05D9, \u05D0\u05D5 \u05DC\u05E2\u05DE\u05D5\u05D3 \u05D1\u05D3\u05E8\
  \u05D9\u05E9\u05D5\u05EA\u2026"
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים הוא התאמת מספר לשלם הקרוב ביותר, או לדיוק עשרוני מסוים. אנו מעגלים מספרים כדי לפשט אותם לקריאת אדם, להפחית עומס חישובי, או לעמוד בדרישות מספריות ספציפיות.

## איך לעשות:
ב-Clojure, אנו משתמשים בעיקר ב-`Math/round`, `Math/floor`, ו-`Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

למקומות עשרוניים ספציפיים, אנו מכפילים, מעגלים וחולקים:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## צלילה עמוקה
לפני שפות תכנות מתוחכמות, עיגול היה תהליך ידני, חשבו על חשבון או נייר. בתכנות, זה קריטי לייצוג מספרים בשל מגבלות הדיוק של מספרים צפים.

חלופות לעיגול כוללות שימוש בכיתת `BigDecimal` לשליטה על הדיוק או ספריות כמו `clojure.math.numeric-tower` לפונקציות מתמטיות מתקדמות. הפונקציה `Math/round` של Clojure מסתמכת על פונקציות ה-`Math.round`, `Math/floor`, ו-`Math/ceil` של Java, כלומר היא נושאת את אותם הנואנסים של float ו-double.

מבחינה יישומית, כאשר מעגלים ב-Clojure, זכרו שהיא באופן אוטומטי משתמשת בדיוק double כאשר עוסקת בעשרוניים. יש להזהר משגיאות עיגול!

## ראו גם
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- הבנה של דיוק מספרים צפים: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
