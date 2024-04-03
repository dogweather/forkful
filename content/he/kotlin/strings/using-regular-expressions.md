---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:58.657460-07:00
description: "\u05D0\u05D9\u05DA \u05DC: #."
lastmod: '2024-03-13T22:44:39.255237-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך ל:


### התאמה בסיסית
כדי לבדוק אם מחרוזת תואמת דפוס מסוים ב-Kotlin, ניתן להשתמש בשיטה `matches` של הכיתה `Regex`.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // פלט: true
```

### מציאה וחילוץ חלקים ממחרוזת
אם אתם רוצים למצוא חלקים במחרוזת שתואמים דפוס, Kotlin מאפשרת לכם לחזור על כל ההתאמות:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Today's date is 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// פלט: 07/09/2023
```

### החלפת טקסט
החלפת חלקים במחרוזת שתואמים דפוס היא פשוטה עם פונקצית ה`replace`:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // פלט: Username: userXXX
```

### פיצול מחרוזות
פיצול מחרוזת לרשימה, באמצעות דפוס regex כמפריד:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // פלט: [1, 2, 3, 4, 5]
```

### ספריות צד שלישי: Kotest
[Kotest](https://github.com/kotest/kotest) היא ספריית בדיקות פופולרית של Kotlin המרחיבה את התמיכה המובנית של Kotlin ב-regex, בעיקר יעילה לוולידציה במקרי בדיקה.

```kotlin
// בהנחה ש-Kotest נוסף לפרויקט שלכם
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// זה יעבור את הבדיקה אם הקלט תואם את דפוס האימייל.
```

על ידי טמיעת ביטויים רגולריים ביישומי Kotlin שלכם, אתם יכולים לבצע עיבוד טקסט מתוחכם בצורה יעילה. בין אם אתם מאמתים קלט ממשתמש, חולצים נתונים או מתמירים מחרוזות, דפוסי regex מציעים פתרון עמיד.
