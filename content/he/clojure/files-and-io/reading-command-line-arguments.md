---
title:                "קריאת פרמטרים משורת הפקודה"
aliases:
- /he/clojure/reading-command-line-arguments/
date:                  2024-01-20T17:55:54.702066-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא תהליך שבו התוכנה שלך לוקחת קלט מהמשתמש מחוץ לקוד. זה חשוב מכיוון שזה מאפשר למשתמשים להעביר נתונים ואופציות לתוכנית ברגע ההפעלה.

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
