---
date: 2024-01-20 17:46:43.774830-07:00
description: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\
  \u05D4 \u05D0\u05E0\u05D5 \u05DE\u05E9\u05D9\u05D2\u05D9\u05DD \u05D7\u05DC\u05E7\
  \ \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D2\u05D3\u05D5\
  \u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\
  \u05D1\u05D5\u05D3 \u05E2\u05DD \u05DE\u05D9\u05D3\u05E2 \u05E1\u05E4\u05E6\u05D9\
  \u05E4\u05D9, \u05DC\u05E0\u05EA\u05D7 \u05D8\u05E7\u05E1\u05D8\u05D9\u05DD, \u05D0\
  \u05D5 \u05DC\u05E9\u05E0\u05D5\u05EA \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD\
  ."
lastmod: '2024-03-13T22:44:39.253684-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4\
  \ \u05D0\u05E0\u05D5 \u05DE\u05E9\u05D9\u05D2\u05D9\u05DD \u05D7\u05DC\u05E7 \u05DE\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D2\u05D3\u05D5\u05DC\
  \u05D4 \u05D9\u05D5\u05EA\u05E8."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## איך לעשות:
ב-Kotlin, יש כמה דרכים לחלץ תת-מחרוזות. הנה כמה דוגמאות.

```Kotlin
fun main() {
    val sentence = "שלום, עולם של קוטלין!"

    // חילוץ תת-מחרוזת באמצעות טווחים (ranges) 
    val greeting = sentence.substring(0..4)
    println(greeting) // ידפיס: שלום

    // חילוץ עם נקודות תחילה וסיום
    val world = sentence.substring(7, 11)
    println(world) // ידפיס: עולם

    // חילוץ עם אינדקסים
    val kotlin = sentence.substringAfter("של ")
    println(kotlin) // ידפיס: קוטלין!
}
```

## עיון מעמיק:
חילוץ תת-מחרוזות אינו תופס חדש; זו פעולה בסיסית בכל שפת תכנות שעוסקת במחרוזות מאז שפות התכנות קיימות. ב-Kotlin, מימוש הפונקציות לחילוץ תת-מחרוזות הוא ישיר וקריא, מותאם ל-idiomatic Kotlin. ישנם אלטרנטיבות כמו regex (ביטויים רגולריים) לחילוצים מורכבים יותר, או שימוש בפונקציות כמו `take()`, `drop()`, ואחרות לקבלת חלקים מסוימים של מחרוזת.

## ראו גם:
- [Kotlin String documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Understanding ranges in Kotlin](https://kotlinlang.org/docs/ranges.html)
