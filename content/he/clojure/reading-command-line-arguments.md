---
title:    "Clojure: תרגם לעברית:קריאת משתני שורת הפקודה"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

אם אתה מתעסק בכתיבת קוד בקלוז'ור, כנראה שפעם נתקלת בצורך לקרוא נתונים מפרמטרי שורת הפקודה (command line arguments). הפעולה הזאת נחשבת לנפוצה במערכת ההפעלה UNIX, אך היא רלוונטית גם למערכות ההפעלה האחרות. למה אנו עושים זאת? וכיצד ניתן לעשות את זה בקוד קלוז'ור?

## כיצד לעשות זאת

השיטה הפשוטה ביותר לקריאת נתונים מפרמטרי שורת הפקודה היא להשתמש בפונקציה *command-line-args*. תחילה ניצור משתנה עם השם *args* שיכיל את כל הארגומנטים שהועברו לתוכנית באמצעות שורת הפקודה. לדוגמה:

```Clojure
(def args *command-line-args*)
```

כעת נוכל להשתמש במשתנה *args* כדי לגשת לארגומנטים השונים. לדוגמה, אם השניים האחרונים מהארגומנטים הם מספרים, נוכל לגשת להם כ-integer על ידי המרה עם הפונקציה *Integer/parseInt*:

```Clojure
(def num1 (Integer/parseInt (last args)))
(def num2 (Integer/parseInt (last (rest args))))
```

כעת נוכל לבצע פעולות מתמטיות עליהם ולהדפיס את התוצאות:

```Clojure
(println num1 "+ " num2 " = " (+ num1 num2))
(println num1 "* " num2 " = " (* num1 num2))
```

לשם נוחות, ניתן להעביר את הארגומנטים ישירות לפונקציה, כמו כן ניתן להוסיף כל מספר של פונקציות על הנתונים שקיבלנו:

```Clojure
(println (apply + args))
(println (apply * args))
(println (apply max args))
```

בנוסף, אם אתם רוצים לקבל דיווחים וטיפול בשגיאות שיכולות להתרחש במהלך קריאת הנתונים מפרמטרי שורת הפקודה, ניתן להשתמש בפונקציות כמו *try* ו-*catch