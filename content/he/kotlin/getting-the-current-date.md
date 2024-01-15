---
title:                "לקבלת התאריך הנוכחי"
html_title:           "Kotlin: לקבלת התאריך הנוכחי"
simple_title:         "לקבלת התאריך הנוכחי"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# מדוע

בעולם התכנות, קיימות מספר פעולות קבועות שכמעט כל תוכנית תצטרך לבצע. אחת מהן היא לקבל את התאריך הנוכחי. זה יכול לשמש במגוון של מטרות, כגון תחילת סדר הלוגיקה, צביעה של חלקי תוכנית, וכו'. מאחר שזה פעולה כל כך נפוצה, חשוב לדעת איך לבצע את זה בצורה המתאימה ביותר בשפת קוטלין.

## איך לעשות

כדי לקבל את התאריך הנוכחי בקוד, ניתן להשתמש בפעולה getCurrentDate() של הספרייה המובנית של קוטלין. הנה דוגמה של כיצד להשתמש בפעולה זו ותוצאת הפלט:

```Kotlin
val currentDate = getCurrentDate()
println("התאריך הנוכחי הוא: $currentDate")
```

פלט:

```
התאריך הנוכחי הוא: דצמבר 1, 2021
```

## חקירה עמוקה

כפי שראינו, פעולת getCurrentDate() מחזירה תאריך מלא כולל חודש, יום ושנה. אבל אם תרצו לקבל רק אחד מהאיברים של התאריך, כגון השנה בלבד, תוכלו לשנות את טיפוס המשתנה של התוצאה. ניתן גם להשתמש בפונקציות נוספות כדי לעצב את התאריך בצורה ייחודית יותר.

## ראו גם

- פונקציות ומתודות מובנות של קוטלין: https://kotlinlang.org/docs/functions.html
- פעולת getCurrentDate() במדריך של קוטלין: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-time/get-current-date.html