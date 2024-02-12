---
title:                "שימוש בביטויים רגולריים"
aliases:
- /he/kotlin/using-regular-expressions.md
date:                  2024-02-03T19:17:58.657460-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
