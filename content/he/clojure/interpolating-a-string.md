---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

החילופים במחרוזת (String Interpolation) הם תהליך שבו משלבים מחרוזות וביטויים משתנים, לעיתים גם באמצעות חישובים, בתוך מחרוזת אחת מרכזית. אנחנו, מתכנתים, עושים זאת על מנת לפשט ולקצר את הקוד שלנו, ולהניע טעויות מיותרות בגביל המחרוזת.

## איך אנחנו עושים זאת?

בשפת תכנות Clojure, אנחנו משתמשים בפונקציה format, כדי לממש Interpolation.

```Clojure
(defn hello [name]
  (format "שלום %s!" name))

(hello "יוגש")
```

פלט:
```Clojure
"שלום יוגש!"
```
ה-fn החוזר ישתמש בשם שהוזן וישלב אותו יחד עם מחרוזת הברכה.

## הצצה עמוקה

Href-interpolation הוא תהליך שמתרחש ברוב שפות התכנות, חלקן תומכות בה באופן מובנה וחלקן דרך פונקציות. ב-Clojure, אנחנו משתמשים בפונקציה 'format'.

שפות תכנות אחרות יכולות להשתמש בחלופות אחרות, כמו String Templates ב-Java או String Interpolation של שפת Python.

בנוגע לפרטי המימוש, Clojure מתרגמת את הצורה של גוף הפונקציה לביטוי JVM bytecode, שמאפשר לה כמה ממשקים, בלי להקריב ביצועים.

## ראה גם:

- [Clojure for the Brave and True: מודולים ו-Namespaces](https://www.braveclojure.com/organization/)׃ בואו נלמד על מדוע ההקפצה מסביב ל-Namespace כל כך חשובה.
- [Clojure By Example: Data Types](https://kimh.github.io/clojure-by-example/#data-types)׃ סקירת הסוגים השונים של נתונים המתממשים ל-Charseq.
- [Clojure: Learn the Syntax](https://clojure.org/guides/learn/syntax)׃ למדו על התחביר של Clojure, עם דוגמאות.