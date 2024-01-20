---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# מציאת אורך שלמיטה ב-Kotlin

## מה ולמה?
מציאת אורך של מחרוזת היא התוצאה המספרית של מספר התווים בתוך המחרוזת. מתכנתים עשויים לזקוק לכך בשביל לבצע גילוף מחרוזת, לבדוק תקינות קלט או לבצע ריכוז שפתים.

## איך לעשות:
הקוד הפשוט הבא ממומש ב-Kotlin:
```Kotlin
fun main(args: Array<String>) {
    val str = "אני אוהב לתכנת ב-Kotlin"
    val length = str.length
    println("אורך המחרוזת: $length")
}
```
מוצא את אורך המחרוזת ומדפיס את התוצאה.

## ביצוע עמוק יותר:
בהיסטוריה, הוגים מגוונים של שפת תכנות בצ’אלט קדימה להספק למתכנתים דרכים שונות למצוא את אורך של מחרוזת. Kotlin עצמה מרה ושלה על התוסף `.length`, שמספק את אורך המחרוזת בצורה ישירה ואינטואיטיבית. חלופות אחרות כוללות שימוש בלולאה לספירת התווים או שימוש בפונקציות מובנות בשפות אחרות. 

מבחינת עלות המילואה, `.length` אינו יעיל יותר בהכרח מאשר חלופות אחרות בעבודה עם מחרוזות הקיימות ברשימות נתונים חסרות סמנים או בעבודה עם מחרוזות גדולות במיוחד.

## לקריאה נוספת:
1. [דוקס הרשמי של Kotlin על מחרוזות](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
2. [רשימת שיעורים מסודרת על מחרוזות ב-Kotlin](https://www.programiz.com/kotlin-programming/string)
3. [StackOverflow: איך למצוא את אורך מחרוזת בקוטלין](https://stackoverflow.com/questions/36894315/how-to-find-a-string-length-in-kotlin)