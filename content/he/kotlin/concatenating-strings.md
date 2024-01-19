---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?

תרגול של מחרוזות הוא התהליך של שרשור שני שמות מחרוזות או יותר למחרוזת אחת. מתכנתים עושים את זה ללכידה והדפסה של נתונים, או מחרוזות דינאמי.

## הדרך לעשות

ב- Kotlin, ניכר שמדובר פה בשפה שנכתבה במחשבה על ההנאה של המפתח. בואו נראה כמה דרכים פשוטות אבל חזקות לשרשר מחרוזות.

```kotlin
fun main() {
    val firstString = "שלום, "
    val secondString = "עולם!"
    val concatString = firstString + secondString
    println(concatString) // פלט: שלום, עולם!
}
```

דרך אחרת תהיה להשתמש ב-string interpolation:

```kotlin
fun main() {
   val name="אזרח"
   println("שלום, $name") // פלט: שלום, אזרח
}
```

## צלילה עמוקה 

תרגול מחרוזות הוא חלק בלתי נפרד מההיסטוריה של שפות התכנות, החל מ-FORTRAN ועד ל-Kotlin. ב-Kotlin, יצירת מחרוזת יכולה להתבצע בצורות רבות שונות, כולל שרשור (concatenation), טמלטים (templates), או פרמט תוך שימוש ב-interpolation.

בחרת בתרגול או ב-interpolation תלוי בתוכנה ורצי המפתח. למרות שתוצאתם שווה, השימוש ב-interpolation יכול להקל על קריאת הקוד.

הוספת תווים למחרוזת קיימת (via concatenation) הופך את הפעולה לאיטית בהשוואה ליצירת מחרוזת חדשה – במיוחד במקרים שבהם ישנם משתנים רבים.

## ראה גם

1. תיעוד Kotlin לגבי מחרוזות: https://kotlinlang.org/docs/strings.html
2. כתיבה נכונה של קוד ב-Kotlin: https://kotlinlang.org/docs/coding-conventions.html
3. בהערכה של string templates: https://kotlinlang.org/docs/tutorials/kotlin-for-py/strings.html#string-templates