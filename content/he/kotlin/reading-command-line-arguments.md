---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:56:55.156071-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא איך אתה מאפשר לתוכנה לקבל מידע מהמשתמש בזמן הרצה. זה חיוני כאשר אתה רוצה לכוונן התנהגות תוכנה מבלי לשנות את הקוד.

## איך לעשות:
מדגים קוד ופלט לדוגמא בתוך בלוקי קוד של ```Kotlin```:

```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("הי, הארגומנט הראשון שלך הוא: ${args[0]}")
    } else {
        println("לא קיבלתי שום ארגומנטים :(")
    }
}
```

הרצה:
```
$ kotlinc Hello.kt -include-runtime -d Hello.jar
$ java -jar Hello.jar שלום
הי, הארגומנט הראשון שלך הוא: שלום
```

## טבילה עמוקה
בתחילת עידן המחשבים, שורת הפקודה הייתה האינטרפייס העיקרי להתקשרות אנושית-מכונה. כיום, פחות נעשה בה, אבל יש מקרים שבהם השימוש בה בלתי נמנע: סקריפטים, כלים אוטומטיים ועברית פקודה. חלופות כוללות קבצי קונפיגורציה, ממשקי משתמש גרפיים או אינטראקציה מתוך התוכנה עצמה עם APIs.

בקוטלין, ארגומנטי שורת פקודה נקראים דרך הפונקציה `main` שמקבלת מערך של מחרוזות. בדגם למעלה, `args` מייצג את הארגומנטים, והגישה לנתונים היא אינדקסית. יש לשים לב לטפל במקרים שבהם לא ניתן כל ארגומנט.

## ראה גם
- [Kotlin Documentation - Basic Syntax](https://kotlinlang.org/docs/basic-syntax.html#program-parameters)
- [Command-line Argument Parsing in Kotlin with kotlinx.cli](https://github.com/Kotlin/kotlinx-cli)