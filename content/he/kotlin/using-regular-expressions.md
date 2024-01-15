---
title:                "שימוש בביטויים רגולריים"
html_title:           "Kotlin: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

בעזרת ביטויי ה-regular expressions אפשר לבצע פעולות מתקדמות על טקסט, כגון חיפוש, תיקון וכתיבת קוד. בנוסף, הם יכולים לחסוך זמן רב במילוי טפסים ועיבוד מידע.

## איך לעשות זאת

תחילה נצטרך ליצור משתנה מסוג Regex המייצג את הביטוי הרגיל שנרצה לחפש. לדוגמה, אם נרצה למצוא את כל המספרים בטקסט, נצטרך להשתמש בביטוי הרגיל \d+. אפשר לבצע זאת באמצעות הפקודה Regex(regexString) כאשר regexString הוא הביטוי הרגיל שרצינו לחפש. לאחר מכן, נוכל להשתמש בפקודת find או findAll על הטקסט שברצוננו לחפש כדי למצוא את ההתאמות המתאימות. לדוגמה:

```Kotlin
val regex = Regex("\\d+")
val text = "Hello 123 World 456"
val matches = regex.findAll(text)
 
for (match in matches) {
    println(match.value)
}
 
// Output: 123 456
```

ניתן גם להשתמש בביטויי הרגיל לתיקון או החלפת טקסט. לדוגמה, אם נרצה להחליף את כל התאריכים בטקסט לתאריכים אחרים, נוכל להשתמש בפקודת replace כדי לחלוף על כל ההתאמות ולהחליף אותן בתאריכים שרצינו. לדוגמה:

```Kotlin
val regex = Regex("(\\d{2})-(\\d{2})-(\\d{4})")
val text = "Today's date is 06-08-2020"
val newDate = regex.replace(text, "$2/$1/$3")

println(newDate)

// Output: Today's date is 08/06/2020
```

## Deep Dive

בעזרת ביטויי הרגיל, ניתן לבצע חיפוש מתקדם על טקסט ולמצוא מידע מאוד מפורט כמו טלפונים, כתובות אימייל או אפילו מבני קבצים. ניתן גם להשתמש בתנאיים כדי לבצע חיפושים מתקדמים יותר, כגון חיפוש תבניות ומילים מסוימ