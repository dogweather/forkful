---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:58.657460-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D4\u05DD \u05DB\u05DC\u05D9 \u05E2\u05D5\u05E6\u05DE\
  \u05EA\u05D9 \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8, \u05D4\
  \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DC\u05D7\u05E4\u05E9, \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D5\u05DC\
  \u05E0\u05D4\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E2\u05DD \u05D8\
  \u05DB\u05E0\u05D9\u05E7\u05D5\u05EA \u05EA\u05D9\u05D0\u05D5\u05DD \u05D3\u05E4\
  \u05D5\u05E1 \u05DE\u05EA\u05E7\u05D3\u05DE\u05D5\u05EA. \u05D1-Kotlin, \u05E9\u05D9\
  \u05DE\u05D5\u05E9 \u05D1-regex\u2026"
lastmod: '2024-03-13T22:44:39.255237-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D4\u05DD \u05DB\u05DC\u05D9 \u05E2\u05D5\u05E6\u05DE\
  \u05EA\u05D9 \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8, \u05D4\
  \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DC\u05D7\u05E4\u05E9, \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D5\u05DC\
  \u05E0\u05D4\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E2\u05DD \u05D8\
  \u05DB\u05E0\u05D9\u05E7\u05D5\u05EA \u05EA\u05D9\u05D0\u05D5\u05DD \u05D3\u05E4\
  \u05D5\u05E1 \u05DE\u05EA\u05E7\u05D3\u05DE\u05D5\u05EA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## מה ולמה?

ביטויים רגולריים (regex) הם כלי עוצמתי לעיבוד טקסט, המאפשרים למתכנתים לחפש, להתאים ולנהל מחרוזות עם טכניקות תיאום דפוס מתקדמות. ב-Kotlin, שימוש ב-regex מסייע לבצע ביעילות משימות עיבוד טקסט מורכבות כמו ולידציה, ניתוח או התמרה, והוא בלתי נפרד למשימות החל מניהול מחרוזת פשוט ועד לניתוח טקסט מורכב.

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
