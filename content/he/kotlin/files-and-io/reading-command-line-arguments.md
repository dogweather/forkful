---
date: 2024-01-20 17:56:55.156071-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D0\u05D9\u05DA \u05D0\u05EA\u05D4 \u05DE\u05D0\u05E4\u05E9\
  \u05E8 \u05DC\u05EA\u05D5\u05DB\u05E0\u05D4 \u05DC\u05E7\u05D1\u05DC \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D6\u05DE\u05DF\
  \ \u05D4\u05E8\u05E6\u05D4. \u05D6\u05D4 \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DB\u05D0\
  \u05E9\u05E8 \u05D0\u05EA\u05D4 \u05E8\u05D5\u05E6\u05D4 \u05DC\u05DB\u05D5\u05D5\
  \u05E0\u05DF \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA \u05EA\u05D5\u05DB\u05E0\
  \u05D4 \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05D4\
  \u05E7\u05D5\u05D3."
lastmod: '2024-03-11T00:14:12.747079-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D0\u05D9\u05DA \u05D0\u05EA\u05D4 \u05DE\u05D0\u05E4\u05E9\
  \u05E8 \u05DC\u05EA\u05D5\u05DB\u05E0\u05D4 \u05DC\u05E7\u05D1\u05DC \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D6\u05DE\u05DF\
  \ \u05D4\u05E8\u05E6\u05D4. \u05D6\u05D4 \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DB\u05D0\
  \u05E9\u05E8 \u05D0\u05EA\u05D4 \u05E8\u05D5\u05E6\u05D4 \u05DC\u05DB\u05D5\u05D5\
  \u05E0\u05DF \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA \u05EA\u05D5\u05DB\u05E0\
  \u05D4 \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05D4\
  \u05E7\u05D5\u05D3."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
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
