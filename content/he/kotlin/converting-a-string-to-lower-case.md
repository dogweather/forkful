---
title:    "Kotlin: המרת מחרוזת לאותיות קטנות"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה
מדוע כדאי להמיר מחרוזת לאותיות קטנות בקוד קוטלין?

מימוש מחרוזת עם אותיות גדולות וקטנות יכול להיות מסובך ולגרום לבעיות בתכנות. השימוש באותיות קטנות לאורך כל הקוד יכול להפחית את סיכויי השגיאות ולהקל על הקריאה של הקוד.

## כיצד לעשות זאת
הנה דוגמאות להמרת מחרוזת לאותיות קטנות בקוד קוטלין:

```Kotlin
val name = "JORGE"
val lowercaseName = name.toLowerCase()
println(lowercaseName) // פלט: jorge
```

או באמצעות פעולה כמו `toLowerCase()`:

```Kotlin
val word = "HELLO"
println(word.toLowerCase()) // פלט: hello
```

## חקירה מעמיקה
בקוד קוטלין, קיימת פונקציה מובנית להמרת מחרוזת לאותיות קטנות בשם `toLowerCase()`. ניתן להשתמש בפונקציה זו כדי להמיר בקלות מחרוזת לאותיות קטנות.

אם נבקר ברקורסיה של הפונקציה `toLowerCase()` נמצא שהיא משתמשת בפעולת `toLowerCase()` מכלי המחרוזות שקיימים בקוד קוטלין. כלומר, הפעולה משלבת את כלי המחרוזת העולמיים בקוד קוטלין כדי להמיר את האותיות לקטנות.

למידע נוסף על הפונקציה `toLowerCase()` וכיצד היא מתוממת בקוד קוטלין, ניתן לגשת לתיעוד של השפה.

## ראה גם
- תיעוד של Kotlin על הפעולה `toLowerCase()` - https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html.