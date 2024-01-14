---
title:    "Clojure: מחיקת תווים התואמים לתבנית."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

מחיקת תווים התואמים לתבנית היא כלי חשוב כאשר אתה מעורב בעיבוד טקסט בקוד Clojure. השימוש בפונקציות כמו `clojure.string/replace` או `clojure.string/replace-first` יכול לסייע לך למחוק תווים שאינם רלוונטיים או ליישר מבנה של טקסט.

## כיצד לעשות זאת

```Clojure
;; מחיקת כל האותיות הגדולות מהטקסט
(clojure.string/replace "Hello World!" #"[A-Z]" "")

;; חילוץ תווים רק מבין המספרים השלמים
(clojure.string/replace-first "abc123def456" #"[^\d]" "")

;; החלפת תווים עם רצף מללי
(clojure.string/replace "I love apples" "apples" "oranges")

```

### תוצאות

```Clojure
"ello orld!" ;; נמחקו כל האותיות הגדולות
"123456" ;; ניתן רק את המספרים של טקסט
"I love oranges" ;; נמצא והוחלף המילה "apples"
```

## מעמיק לתוך

כאשר משתמשים בפונקציות למחיקת תווים, חשוב להיות ערני לגבי התבנית שאתה משתמש בה. למשל, הביטוי #"[A-Z]" יתאים לכל אותיות גדולות בטקסט, והביטוי #"[^^]" ימחק את כל התווים שאינם מתאימים לטקסט.

בנוסף, כדאי להשתמש בפונקציות נתמכות על-ידי ספריית `clojure.string`, כמו `replace` ו `replace-first`. אל תשכח לבדוק את רשימת הפונקציות הנתמכות כדי למצוא את הכלי המתאים ביותר עבור המשימה שלך.

## ראה גם

- [ספריית Clojure.string במקראט oClojure](https://clojuredocs.org/clojure.string) 
- [מאמר על סטנטרדיות בספריית Clojure.string](https://purelyfunctional.tv/guide/clojure-string/) 
- [דוגמאות מתקדמות לשימוש בספריית Clojure.string](https://gist.github.com/borkdude/195fb4ab09fbab5def164ea096417811)