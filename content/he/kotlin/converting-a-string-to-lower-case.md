---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Kotlin: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

אתה יכול להיות מתהדר עם הקוד שלך כמה שברצונך, אבל האמת היא שלעיתים קרובות אתה תצטרך להתמודד עם נתונים מסוג שונה שמגיעים בפורמטים שונים. כשאתה מעבד מחרוזת בקוד שלך, לעיתים קרובות תרצה להביא אותה לפורמט מסוים, כמו כתיבה או קריאה בינלאומית. אחת השיטות לעשות זאת היא להמיר את המחרוזת לאותיות קטנות (Lower Case). זה יקל עליך לבצע רטרייבל ועיבוד נתונים עתידיים.

## איך

```Kotlin
str.toLowerCase()
```

כיוון שבקוטלין המחרוזות הן immutable (לא ניתן לשנותן), הפעולה לא משנה את המחרוזת המקורית אלא מחזירה מחרוזת חדשה בפורמט שאתה רוצה. הנה דוגמא לפקודה זו:

```Kotlin
val str = "Hello World"
val lowercaseStr = str.toLowerCase()
```

ישנן גם פקודות נוספות לסירוגין לתרגום של מחרוזת לאותיות קטנות. הנה דוגמאות לכמה מהנקודות האלה:

```Kotlin
// כתיבת אותיות נתמךות בכל תווי התווית
"Hello World".toLowerCase() // hello world
"Привет мир".toLowerCase() // привет мир
"こんにちは世界".toLowerCase() // こんにちは世界

// תוויות עם טאבים
"Hello\tWorld".toLowerCase() // hello\tworld

// תוויות עם טאבים לחפוש
"Hello World".toLowerCase(Locale.ROOT) // hello world
"Привет мир".toLowerCase(Locale.ROOT) // привет мир
"こんにちは世界".toLowerCase(Locale.ROOT) // こんにちは世界
```

## Deep Dive

בקוטלין, כשאתה משתמש בפעולת `toLowerCase()`, אתה משתמש בפונקציה של מחלקת String. מחלקת String כוללת מספר פונקציות נוספות לסירוגין, כמו `toUpperCase()` ו- `replace()`. הפעולה `toLowerCase()` משתמשת בפונקציה חיצונית בשם `toLowerCase_internal()` המתמודדת עם הדגש והניקוד השונים בנתונים.