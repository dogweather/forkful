---
date: 2024-01-26 01:10:31.098452-07:00
description: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\
  \u05DA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05D6\u05D4 \u05E2\u05E0\
  \u05D9\u05D9\u05DF \u05E9\u05DC \u05D0\u05E8\u05D9\u05D6\u05EA \u05D1\u05DC\u05D5\
  \u05E7\u05D9\u05DD \u05E9\u05DC \u05E7\u05D5\u05D3 \u05E9\u05DE\u05D1\u05E6\u05E2\
  \u05D9\u05DD \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05E1\u05E4\u05E6\u05D9\u05E4\
  \u05D9\u05D5\u05EA. \u05E2\u05E9\u05D9\u05D9\u05D4 \u05D6\u05D5 \u05D2\u05D5\u05E8\
  \u05DE\u05EA \u05DC\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05DC\u05D4\u05D9\u05D5\
  \u05EA \u05E0\u05E7\u05D9, \u05E7\u05DC \u05D9\u05D5\u05EA\u05E8 \u05DC\u05EA\u05D7\
  \u05D6\u05D5\u05E7\u05D4 \u05D5\u05E0\u05D5\u05D7 \u05DC\u05E7\u05E8\u05D9\u05D0\
  \u05D4 \u05E2\u05D1\u05D5\u05E8\u2026"
lastmod: '2024-03-13T22:44:38.713481-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05D6\u05D4 \u05E2\u05E0\u05D9\
  \u05D9\u05DF \u05E9\u05DC \u05D0\u05E8\u05D9\u05D6\u05EA \u05D1\u05DC\u05D5\u05E7\
  \u05D9\u05DD \u05E9\u05DC \u05E7\u05D5\u05D3 \u05E9\u05DE\u05D1\u05E6\u05E2\u05D9\
  \u05DD \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9\
  \u05D5\u05EA. \u05E2\u05E9\u05D9\u05D9\u05D4 \u05D6\u05D5 \u05D2\u05D5\u05E8\u05DE\
  \u05EA \u05DC\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05DC\u05D4\u05D9\u05D5\u05EA\
  \ \u05E0\u05E7\u05D9, \u05E7\u05DC \u05D9\u05D5\u05EA\u05E8 \u05DC\u05EA\u05D7\u05D6\
  \u05D5\u05E7\u05D4 \u05D5\u05E0\u05D5\u05D7 \u05DC\u05E7\u05E8\u05D9\u05D0\u05D4\
  \ \u05E2\u05D1\u05D5\u05E8\u2026"
title: "\u05E1\u05D9\u05D3\u05D5\u05E8 \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

ארגון קוד לתוך פונקציות זה עניין של אריזת בלוקים של קוד שמבצעים משימות ספציפיות. עשייה זו גורמת לקוד שלך להיות נקי, קל יותר לתחזוקה ונוח לקריאה עבור מפתחים אחרים.

## איך לעשות:

פונקציות ב-Clojure מוגדרות בעזרת `defn`, אחריו מגיעים שם, פרמטרים, וגוף הפונקציה. הנה דוגמה מהירה.

```Clojure
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alex") ; => "Hello, Alex!"
```

נניח עכשיו שאנחנו רוצים לחשב את שטחו של מלבן. במקום לבלגן הכל יחד, אנחנו מפרידים את זה לשתי פונקציות:

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "The area is:" (area length width)))

(print-area 3 4) ; => The area is: 12
```

## צלילה עמוקה

בעבר, מתכנתים היו פשוט להכניס את כל הלוגיקה שלהם לתוך בלוק אחד. זה היה מכוער. אז הגיע תכנות מובנה, ופונקציות הפכו לדבר. ב-Clojure, כל פונקציה היא מעולה – אתה יכול להעיף אותן סביב כמו ערך אחר כלשהו.

אלטרנטיבות? כמה אנשים עשויים להתעסק עם מתודות רבות או פונקציות מסדר גבוה, אבל אלו רק תבלינים בתבשיל הפונקציות.

כל מה שבפרטי הפונקציה: הן אי-שינוייות ב-Clojure, דבר שמפחית את הסיכון לבלגנים עקב תופעות לוואי. הן מתבססות רבות על רקורסיה במקום לולאות טיפוסיות, שמתמזג היטב עם הפרדיגמות הפונקציונליות של השפה.

## ראה גם

- המדריך העצמי של Clojure: https://clojure.org/guides/learn/functions
- יסודות תכנות פונקציונלי: https://www.braveclojure.com/core-functions-in-depth/
- הרצאות של Rich Hickey: https://changelog.com/posts/rich-hickeys-greatest-hits - להבנת פילוסופיית Clojure.
