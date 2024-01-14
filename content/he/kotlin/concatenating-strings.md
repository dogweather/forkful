---
title:                "Kotlin: לשרשור מחרוזות"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## מדוע

התפתחות תוכנות המחשב והאינטרנט הביאו לצורך חזק בתהליך של חיבור מחרוזות. ניתן להשתמש בתהליך זה כדי ליצור מחרוזות ייחודיות ומותאמות לצרכי התוכנה שלנו. חיבור מחרוזות הוא פעולה חשובה לתכונות רבות כגון תרגום, עיבוד מקלדת והצגת מידע למשתמשים.

## כיצד לעשות זאת

החיבור של מחרוזות ב-Kotlin נעשה בעזרת הסימן `+` בין המחרוזות. הנה דוגמא פשוטה:

```Kotlin
val str1 = "שלום"
val str2 = "עולם"
val result = str1 + str2
println(result)
```

פלט:

```
שלוםעולם
```

ניתן גם לחבר בין מחרוזות למשתנים עם הסימן `$`. כך ניתן ליצור מחרוזת דינמית שמשלבת בתוכה את הערך של המשתנה. לדוגמה:

```Kotlin
val name = "דני"
val message = "שלום $name, מה נשמע?"
println(message)
```

פלט:

```
שלום דני, מה נשמע?
```

## משולב עמוק

ב-Kotlin ישנם גם פעולות נוספות לחיבור מחרוזות. למשל, ניתן להשתמש בפעולת `plus()` כדי לחבר בין מחרוזות וליצור מחרוזת חדשה. הנה דוגמא:

```Kotlin
val str1 = "abc"
val str2 = "def"
val result = str1.plus(str2)
println(result)
```

פלט:

```
abcdef
```

בנוסף, ניתן להשתמש בפעולת `plusAssign()` כדי להוסיף מחרוזת למחרוזת קיימת. לדוגמה:

```Kotlin
var str = "abc"
val str2 = "def"
str += str2
println(str)
```

פלט:

```
abcdef
```

## ראה גם

- [תיעוד רשמי לחיבור מחרוזות ב-Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#string-concatenation)
- [התחברות לכל פתרונות הקוד הפתוח מבית JetBrains](https://github.com/JetBrains/kotlin)