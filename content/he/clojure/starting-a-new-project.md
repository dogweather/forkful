---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?

אתחול פרויקט חדש הוא המהלך הראשון שבו אתה מחליט ליצור תוכנה או אפליקציה באמצעות שפת התכנות Clojure. תכנתים מתחילים פרויקטים כדי ליישם רעיונות חדשים, לנתח מחדש בעיות ולבנות פתרונות אותם סף החוויה שלהם מאפשר.

## איך ל:

נתחיל בבניית פרויקט חדש. על מנת ליצור פרויקט חדש של Clojure, נשתמש בכלי Leiningen.
```Clojure
lein new myproject
```
אתה צריך לראות מספר קבצים ותיקיות שנוצרו:
```Clojure
myproject/
|- .gitignore
|- project.clj
|- README.md
|- doc/
|  |- intro.md
|- resources/
|- src/
|  |- myproject/
|     |- core.clj
|- test/
   |- myproject/
      |- core_test.clj
```
## Deep Dive 

Clojure החלה לפתח ב-2007, והיא שפת תכנות פונקציונלית המתמקדת בתכנות תרחיש מערכת. היא מספקת כלים למניעת בעיות שמתרחשות בעת כתיבת קוד ברוב השפות הלא פונקציונליות. 

חלופות ל-Clojure כוללות שפות פונקציונאליות אחרות כמו Haskell, Erlang, F#, ואחרות. אך Clojure מציעה שילוב ייחודי של תנאים: גישה סידורתית, מבנה נתונים אי שנוי, אינטרופיה לJVM עם גישה מלאה ל-API של Java.

## ראה גם

1. [תיעוד של Clojure](https://clojure.org/guides/getting_started)
2. [Leiningen גייד](https://leiningen.org/)
3. [Clojure ב-Codecademy](https://www.codecademy.com/learn/learn-clojure)