---
title:                "סידור קוד לתוך פונקציות"
aliases:
- /he/clojure/organizing-code-into-functions/
date:                  2024-01-26T01:10:31.098452-07:00
model:                 gpt-4-1106-preview
simple_title:         "סידור קוד לתוך פונקציות"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/organizing-code-into-functions.md"
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
