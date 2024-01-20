---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
ליצור פרויקט חדש בתכנות, זו ההתחלה של כל תהליך. זה הדרך שלנו לארגן את הקוד שלנו לחתיכות המנוהלות וממוקדות. תכניתאים מתחילים פרויקטים חדשים כדי ליצור מוצרים חדשים, להשפיע על השינויים הטכנולוגיים, או פשוט ללמוד טכנולוגיות חדשות.

## איך לעשות:
כאן דוגמה לאיך ליצור פרויקט חדש באמצעות Kotlin:

```Kotlin
// Set up the project
$ mkdir MyNewProject
$ cd MyNewProject
$ touch main.kt

// Edit the main.kt file
$ nano main.kt

// Add this to the file
fun main() {
    println("Welcome to My New Project!")
}

// Save and exit nano, then compile and run
$ kotlinc main.kt -include-runtime -d main.jar
$ java -jar main.jar
```

כאשר אתה מריץ את הקוד, הפלט צפוי להופיע:

```Shell
Welcome to My New Project!
```

## ביצוע נסיעה מעמיקה:
ההיסטוריה של יצירת פרויקטים בתכנות התחילה מן הפעם הראשונה שמדען מחשבים כתב את הקוד הראשון. מאז, זה התפתח עם כלי חדשים וטכניקות ליצירת פרויקטים באופן מהיר ויעיל יותר.
חלופות לKotlin ליצירת פרויקט חדש יכולות לכלול קוד בשפות אחרות, כמו Java, Python, או JavaScript.
בניית פרויקט באמצעות Kotlin מעריכה את שילוב הוראות JSON וXML בהן מפרטים מגבלות ומסגרות הפרויקט.

## ראה גם:
1. [התיעוד הרשמי של Kotlin](https://kotlinlang.org/docs/home.html)
2. ["קורס למדריך עבודה מעשית עם Kotlin"](https://www.youtube.com/playlist?list=PLQQYSgDznKyvc1xTypQYTDJla1oMh5I-4)