---
title:                "הדפסת פלט לניפוי באגים"
aliases:
- he/clojure/printing-debug-output.md
date:                  2024-01-20T17:52:13.827252-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
דיבאגינג זה להדפיס את פלט הבדיקה לקונסול כדי לראות מה קורה בקוד. תכנתים עושים את זה כדי לאתר בעיות ולתקן אותן.

## How to: (איך לעשות:)
בקלוז'ר, משתמשים ב `println` לצורך הדפסת פלט לקונסול.

```Clojure
;; הדפסת מחרוזת בסיסית
(println "שלום עולם")

;; הדפסת ערכים מרובים
(println "הערך הראשון:" 1 ", הערך השני:" 2)

;; דיבאגינג עם println
(defn add [a b]
  (println "הוספה:" a "+" b)
  (+ a b))

(add 3 4)
```

תוצאת הדוגמה:

```
שלום עולם
הערך הראשון: 1 , הערך השני: 2
הוספה: 3+4
```

## Deep Dive (צלילה עמוקה):
הדפסת פלט נחשבת לטכניקת דיבאגינג מהירה וישירה. בעבר, לדיבאגרים לא הייתה ממשקית משתמש גרפית וההדפסה לקונסול הייתה הבחירה העיקרית. כיום קיימים כלים מתקדמים יותר כמו דיבאגרים מובנים, אך הדפסה עדיין משמשת לבדיקה מהירה ולבעיות בלתי צפויות. בקלוז'ר, `println` נשארת שיטה פופולרית כי היא פשוטה וגמישה לשימוש. חשוב לזכור להסיר את ההדפסות לפני הפצת הקוד.

## See Also (ראו גם):
- [ClojureDocs on println](https://clojuredocs.org/clojure.core/println)
