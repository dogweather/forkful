---
title:                "חיבור מחרוזות"
html_title:           "Kotlin: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה
לכל מתכנת שימושי לדעת איך לחבר מחרוזות, משום שזה דבר כל כך נפוץ וחיוני בתכנות בכל שפה.

## איך לעשות זאת
כדי לחבר מחרוזות ב-Kotlin, נייצר משתנה מסוג `String` ונשתמש באופרטור `+` כדי לחבר בין שתי מחרוזות. לדוגמה:

```Kotlin
val firstString = "Hello "
val secondString = "world!"
val combinedString = firstString + secondString
println(combinedString)
```

פלט:
```
Hello world!
```

כמו כן, ניתן לחבר מחרוזת ישירות לערך של משתנה אחר, בעזרת הפונקציה `plus()` כך:

```Kotlin
val number = 42
val combinedString = "The answer to the ultimate question of life, the universe, and everything is " + number
println(combinedString)
```

פלט:
```
The answer to the ultimate question of life, the universe, and everything is 42
```
## מעמיקים יותר
בנוסף לאופרטור `+`, ב-Kotlin ישנם גם אופרטורים נוספים שמאפשרים לנו לחבר מחרוזות בצורה יעילה יותר. למשל, ניתן להשתמש באופרטור `inc()` כדי לחבר מחרוזת לערך של משתנה מספרי. לדוגמה:

```Kotlin
val number = 7
val string = "The lucky number is "
val result = string + number++
println(result)
```

פלט:
```
The lucky number is 7
```

כמו כן, ניתן להשתמש באופרטור `plusAssign()`, כדי להוסיף מחרוזת לערך של משתנה מסוג `StringBuilder` בצורה יעילה יותר. לדוגמה:

```Kotlin
val builder = StringBuilder("Java")
builder += " is"
builder += " awesome!"
println(builder)
```

פלט:
```
Java is awesome!
```

בנוסף, ב-Kotlin ניתן להשתמש בתבנית מחרוזת (string templates) כדי לחבר בין מחרוזת לבין ערך של משתנה או תוצאת ביטוי. לדוגמה:

```Kotlin
val name = "Alice"
val message = "Hello $name, welcome to Kotlin!"
println(message)
```

פלט:
```
Hello Alice, welcome to Kotlin!
```

## ראו גם
- [תיעוד השפה הרשמי של Kotlin על חיבור מחרוזות](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [מדריך מפורט על תבניות מחרוזת ב-Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- [סרטון הדג