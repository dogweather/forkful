---
date: 2024-01-20 17:52:13.827252-07:00
description: "\u05D3\u05D9\u05D1\u05D0\u05D2\u05D9\u05E0\u05D2 \u05D6\u05D4 \u05DC\
  \u05D4\u05D3\u05E4\u05D9\u05E1 \u05D0\u05EA \u05E4\u05DC\u05D8 \u05D4\u05D1\u05D3\
  \u05D9\u05E7\u05D4 \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC \u05DB\u05D3\u05D9\
  \ \u05DC\u05E8\u05D0\u05D5\u05EA \u05DE\u05D4 \u05E7\u05D5\u05E8\u05D4 \u05D1\u05E7\
  \u05D5\u05D3. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05EA\u05E8 \u05D1\u05E2\
  \u05D9\u05D5\u05EA \u05D5\u05DC\u05EA\u05E7\u05DF \u05D0\u05D5\u05EA\u05DF."
lastmod: 2024-02-19 22:04:57.979940
model: gpt-4-1106-preview
summary: "\u05D3\u05D9\u05D1\u05D0\u05D2\u05D9\u05E0\u05D2 \u05D6\u05D4 \u05DC\u05D4\
  \u05D3\u05E4\u05D9\u05E1 \u05D0\u05EA \u05E4\u05DC\u05D8 \u05D4\u05D1\u05D3\u05D9\
  \u05E7\u05D4 \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC \u05DB\u05D3\u05D9 \u05DC\
  \u05E8\u05D0\u05D5\u05EA \u05DE\u05D4 \u05E7\u05D5\u05E8\u05D4 \u05D1\u05E7\u05D5\
  \u05D3. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05EA\u05E8 \u05D1\u05E2\u05D9\
  \u05D5\u05EA \u05D5\u05DC\u05EA\u05E7\u05DF \u05D0\u05D5\u05EA\u05DF."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
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
