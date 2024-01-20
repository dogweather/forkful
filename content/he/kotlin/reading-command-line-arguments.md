---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה הם דרך למתכנתים לקבל קלט מהמשתמש בזמן ריצה של התוכנה. מתכנתים משתמשים בזה במיוחד כאשר מתחילים פרסס או מתאמים פעולות בהתאם לפקודות המשתמש.

## איך להשתמש כדי:
```Kotlin
fun main(args: Array<String>) {
    for (i in args.indices) {
        println("Argument $i is: ${args[i]}")
    }
}
```
עם הקוד שלמעלה אם אתה מריץ את התוכנה שלך עם `./MyProgram Hello World`, הפלט יהיה:
```Kotlin
Argument 0 is: Hello
Argument 1 is: World
```
## צלילה עמוקה
קריאת ארגומנטים מהשורה היא משהו שמתכנתים עשו מאז הימים הראשונים של שפת המחשב. חלופות נוספות כוללות פרסרים פקודות (המאפשרים למתכנתים ליצור ארגומנטים באמצעות כמה שפות שולחנות) וקבצים תצורה. בדרך כלל ארגומנטים מהשורה משמשים כאשר התוצאה הרצויה ממרות למרות, ומתהדרים בפשטות.

## ראה גם
1. [Kotlin Programming Cookbook](https://www.amazon.com/Kotlin-Programming-Cookbook-Building-Applications/dp/1788472144): מתכנים של תוכנית הקלט לנשלחים של שורת פקודה.
2. [Kotlin Command Line](https://kotlinlang.org/docs/command-line.html): הוראות מהאתר הרשמי של Kotlin. 
3. [GeekForge - Processing Command Line Params](http://geekforge.org/android/kotlin/2018/01/27/processing-command-line-arguments-in-kotlin.html): קוד כולל שלויות, בדיקות קצה ותיעוד.