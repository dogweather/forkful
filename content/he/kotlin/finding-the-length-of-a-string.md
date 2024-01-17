---
title:                "מציאת אורך של מחרוזת"
html_title:           "Kotlin: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
למצוא את אורך המחרוזת הוא פעולה פשוטה ונפוצה במתכנתים. זה משמש לנו לדעת כמה תווים יש במחרוזת כדי להתאים אותה כאחת הכניסות לאלגוריתם או להציג בממשק משתמש.

## איך לעשות זאת:
הנה כמה דוגמאות למחשבון אורך מחרוזת ולפלט הנובע מהתכנית:
```Kotlin
// דוגמא ראשונה: נקבל את אורך המחרוזת באמצעות פעולת length
val text = "מחרוזת לדוגמא"
println(text.length) // הארך הצפוי: 15

// דוגמא שנייה: ממצע אורך של שני מחרוזות עם השימוש בפעולת count
val text1 = "מחרוזת ראשונה"
val text2 = "מחרוזת שנייה"
val averageLength = (text1.length + text2.length)/2
println(averageLength) // האורך הממוצע: 20.5
```

## צלילה מעמיקה:
השיטות המונחות כאן הן רק כמה מהדרכים לקבל את אורך המחרוזת בKotlin. ישנן גם המון פעולות נוספות כמו indexOf ו-substring שיכולות לעזור לנו לעבוד עם מחרוזות בצורה מדויקת יותר. על מנת לקבל פלט נוסף אנו יכולים להשתמש בתוכנות ליידי לעטוף את פעולת length כדי לקבל את המידע על מותאם למשתמש.

## ראה גם:
- [פעולת length שלKotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/length.html)
- [מדריך מתחילים למחרוזות בKotlin](https://www.baeldung.com/kotlin/strings)
- [תיעוד מפורט שלKotlin](https://kotlinlang.org/docs/reference/strings.html)