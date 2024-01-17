---
title:                "קריאת פרמטרים משורת הפקודה"
html_title:           "Clojure: קריאת פרמטרים משורת הפקודה"
simple_title:         "קריאת פרמטרים משורת הפקודה"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

מה ולמה?

קריאת ארגומנטים של שורת פקודה היא תהליך שבו מתכנתים קוראים ועובדים עם פרמטרים שמועברים לתכנית משורת הפקודה. זהו דרך נפוצה להעביר מידע לתוכנית, ואפשר להשתמש בו לשליטה והתאמה ייחודיים של תוכניות.

כיצד לעשות זאת?

לדוגמה, כדי לקרוא ארגומנטים של שורת פקודה בקלות בשפת Clojure, ניתן להשתמש בפונקציה *command-line-args* שנמצאת בתוך ספריית האינטרנט של שפת Clojure. ניתן לראות בתמונה הבאה דוגמאת קוד ופלט.

```Clojure
(def args *command-line-args*)
(println args)
```

```Clojure
> clj -m hello-world hello "world"
(hello-world hello world)
```

פעולה עמוקה

קריאת ארגומנטים של שורת פקודה היא תהליך שנמצא בשימוש כבר מאז תחילת העידן המתכנת. לפני זה, המידע נכנס לתוכניות באמצעות קלט משתמש. יתכן גם להשתמש באופן אחר של קריאת ארגומנטים בשפת Clojure, משמשת לקרוא פרמטרים מתוך קובץ שהוגדר מראש.

המקורות נוספים

למידע נוסף ודוגמאות נוספות בקריאת ארגומנטים של שורת פקודה בשפת Clojure, ניתן לראות בגיטהאב של סרטון הדגמה הבא: https://github.com/clojure/clojure/blob/master/src/clj/clojure/main.clj

ריארו הפרוייקט הרשמי של שפת Clojure: http://clojure.org/