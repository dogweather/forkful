---
title:                "קריאת ארגומנטים בשורת הפקודה"
html_title:           "Clojure: קריאת ארגומנטים בשורת הפקודה"
simple_title:         "קריאת ארגומנטים בשורת הפקודה"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

אנשים משתמשים בפרמטרי שורת פקודה כדי להריץ תכניות בפרמטרים שונים, לתקשורת עם תוכניות אחרות ולהתאים את התכנית לסביבה מסוימת. קריאת הפרמטרים של השורת הפקודה היא כלי חשוב בכדי לאפשר קשר בין המשתמש והתוכנית.

## איך לעשות את זה

אתה יכול לקרוא את הפרמטרים של השורת פקודה באמצעות הפונקציה `command-line-args` בתוך Clojure. להלן דוגמה קצרה כיצד לקרוא את הפרמטרים:

```Clojure
(def args command-line-args)
```

אפשר להדפיס את הפרמטרים באמצעות הפונקציה `print` ולראות את הפלט:

```Clojure
(print args)
```

פלט: `["arg1" "arg2" "arg3"]`

עכשיו אתה יכול לעבור על הפרמטרים בלולאה ולבצע פעולות נוספות כדי להתאים את התוכנית שלך לצורכים מסוימים.

## לחפור עמוק

בנות פרמטרים משורת הפקודה מאפשרים לכם לספק פרמטרים משתנים לתוכנית שלכם, כגון התאמה לסביבה מקומית או התאמה לאינטראקציה עם משתמשים אחרים. לפי כך, כשתפתחו תוכנית, כדאי שתוודאו שהיא מקבלת את הפרמטרים המתאימים ומתאימה את עצמה לסביבה שבה היא מפועלת.

## ראו גם

- [ClojureDocs: `command-line-args`](https://clojuredocs.org/clojure.core/command-line-args)
- [Clojure for the Brave and True: Reading Command Line Arguments](https://www.braveclojure.com/reading-command-line-arguments/)