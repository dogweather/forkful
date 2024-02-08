---
title:                "חילוץ תת-מחרוזות"
aliases:
- he/kotlin/extracting-substrings.md
date:                  2024-01-20T17:46:43.774830-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא פעולה שבה אנו משיגים חלק מתוך מחרוזת גדולה יותר. מתכנתים עושים זאת כדי לעבוד עם מידע ספציפי, לנתח טקסטים, או לשנות פורמטים.

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
