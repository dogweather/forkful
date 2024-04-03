---
date: 2024-01-20 17:55:54.702066-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Clojure \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\
  \u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\
  \u05D5\u05D3\u05D4 \u05D1\u05E7\u05DC\u05D5\u05EA \u05E8\u05D1\u05D4. \u05D1\u05D3\
  \u05D5\u05D2\u05DE\u05D4 \u05D4\u05D1\u05D0\u05D4 \u05E0\u05D3\u05E4\u05D9\u05E1\
  \ \u05D0\u05EA \u05D4\u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05E9\
  \u05D4\u05EA\u05E7\u05D1\u05DC\u05D5."
lastmod: '2024-03-13T22:44:38.730031-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05E7\u05E8\u05D9\u05D0\u05EA\
  \ \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\
  \u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05D1\u05E7\u05DC\u05D5\u05EA \u05E8\
  \u05D1\u05D4."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

## איך לעשות:
Clojure מאפשרת קריאת ארגומנטים משורת הפקודה בקלות רבה. בדוגמה הבאה נדפיס את הארגומנטים שהתקבלו:

```Clojure
(ns example.args)

(defn -main [& args]
  (println "Received arguments:" args))

; הפעלה משורת הפקודה
; clojure -M -m example.args arg1 arg2 arg3
; פלט משוער:
; Received arguments: (arg1 arg2 arg3)
```

אתה יכול להשתמש בארגומנטים אלה כדי להתאים אישית את ההתנהגות של התוכנית שלך.

## הבנה עמוקה יותר
בעבר, שפות כמו C השתמשו במערך ה `argv` כדי לקרוא ארגומנטים, ו-JVM לא הייתה שונה. ב-Clojure, אשר רץ על JVM, אנו משתמשים בגישה פונקציונלית. יתרה מזאת, יש לנו ספריות, כמו `tools.cli`, שמוסיפות אופציות פרסינג מתקדמות.

Clojure לא דורשת הגדרת פונקציה מיוחדת כמו `public static void main` ב-Java. היא משתמשת ב-fn `-main` שכתובה עם `& args` לקליטת כל מספר של ארגומנטים.

ב-Clojure, כל הארגומנטים מתקבלים כמחרוזות, אז אם אתה צריך סוגים נתונים שונים, תצטרך להמיר אותם. ספריות כדוגמת `tools.cli` יכולות לסייע בפרסינג והמרה.

## ראה גם
- [The Clojure CLI Guide](https://clojure.org/guides/deps_and_cli) - מדריך ל-Clojure CLI, כולל למידע על ארגומנטים.
- [tools.cli on GitHub](https://github.com/clojure/tools.cli) - הספרייה `tools.cli` עבור פרסינג ארגומנטים.
- [Clojure Docs](https://clojuredocs.org/) - מסמכים כלליים על Clojure, אינדקס פונקציות, ודוגמאות קוד.
