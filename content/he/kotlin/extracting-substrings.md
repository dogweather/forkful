---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?

ייצוב תת-חריצים הוא התהליך של חילוץ קטע מתוך מחרוזת. מתכנתים ישמשו את זה לעיבוד וחיפוש מידע באמצעות מחרוזות ספציפיות.

## איך לעבוד:

קוד Kotlin כדי לחלץ תת-מחרוזות :
```kotlin
fun main() {
    val str = "ברוך הבא לכותלין!"
    val substr = str.substring(6,14)
    println(substr)
}
```
פלט :
```kotlin
לכותלין
```

## צלילה עמוקה:

1. הקשר ההיסטורי: בעצם, כל שפות התכנות מגיעות עם פונקציות חילוץ של תת-המחרוזות.
2. אלטרנטיבות: בכותלין, ניתן גם להשתמש ב-fun String.slice(IntRange): String להשגת אותו התוצאה.
3. פרטי ביצוע: Kotlin שומרת על מאפיין 'indices' עלמחרוצת שמרשה אינדקסציה מבוססת-טווח.

## ראו גם:

[מסמך התכנות של קוטלין: String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
[מדריך ויקיפדיה עבור קוטלין](https://he.wikipedia.org/wiki/%D7%A7%D7%95%D7%98%D7%9C%D7%99%D7%9F)