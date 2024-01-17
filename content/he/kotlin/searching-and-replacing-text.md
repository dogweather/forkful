---
title:                "חיפוש והחלפת טקסט"
html_title:           "Kotlin: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט היא פעולה נפוצה בתכנות המשמשת למציאת חלק מסוים של טקסט והחלפתו בחלק אחר. תוכניתיסטים מחפשים ומחליפים טקסט בכדי לתקן שגיאות או לשנות חלקים מסוימים בתוכניות שלהם.

## איך לעשות?
כדי לחפש ולהחליף טקסט בשפת קוטלין, ניתן להשתמש בפונקציות built-in כגון `replace`, `replaceFirst` ו-`replaceAll` כמו בדוגמאות הבאות:

```Kotlin
var originalText = "Hello, my name is John"
var newText = originalText.replace("John", "Jane")
println(newText)

// Output: Hello, my name is Jane
```

```Kotlin
var originalText = "Hello, my name is John, and I am from England"
var newText = originalText.replaceFirst("John", "Jane")
println(newText)

// Output: Hello, my name is Jane, and I am from England
```

```Kotlin
var originalText = "Hello, my name is John, and I am from England"
var newText = originalText.replaceAll("John", "Jane")
println(newText)

// Output: Hello, my name is Jane, and I am from England
```

## חקירה מעמיקה
חיפוש והחלפת טקסט נמצאים בשימוש רב בתכנות, עם דוגמאות ויצירת קשרים רבים. אחת משימושיותו העיקריות שלו היא למצוא ולתקן שגיאות לתוכניות קיימות, כך שהוא יכול להיות כלי יעיל לפיתוח ותחזוקת תוכניות.

קיימים גם כמה ביטויי-רגל ודפוסים חוקיים המשמשים לחיפוש והחלפת טקסט. אחד הפתרונות התכנותיים הפופולריים לחיפוש טקסט הוא שימוש בפונקציות מתוך ספריית חיצונית.

במיקור חוץ משימושים נפוצים לחיפוש והחלפת טקסט בתכנות, תוכניתיסטים יכולים להשתמש לעיתים קרובות בשיטת "חיפוש והחלפה לפי כללים" המאפשרת לחלוף על כמה מילים בכל המופיעים בטקסט באופן אוטומטי.

## ראה גם
- [פונקציות בנויות בשפת קוטלין](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [ביטויים רגילים ודפוסים חוקיים](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)