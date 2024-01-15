---
title:                "חילוץ תת-מחרוזות"
html_title:           "Kotlin: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

למה להתעסק בחילוץ מחרוזת? טוב, ברשימה אתה

1. עובד עם נתונים וזיהוי עיקריים לניתוח נתונים.
2. צריך לשלב מחרוזות כחלק ממטלות כמו תיאור תכניות או פילטר מסננים.

## איך לעשות זאת

ראשית, נצטרך להעדפת מחרוזת ראשונית, וכדי לעשות זאת, נשתמש בפעולה `subSequence()`. למשל, ננסה להחליף את האות "א" במחרוזת "Hello אותו":

```Kotlin
val originalString = "Hello World"
val replacedString = originalString.replace("World", "Place")

println(replacedString)
```
Output: Hello Place

כעת נרצה לחלץ את האות "א" מהמחרוזת המקורית:

```Kotlin
val originalString = "Hello World"
val substring = originalString.subSequence(6,7)

println(substring)
```
Output: א

ניתן גם להשתמש בפעולה `substring()` כדי לחלץ עוד יותר מחרוזות. בדוגמה הבאה, נחלץ את המספרים במחרוזת "12345":

```Kotlin
val originalString = "12345"
val substring1 = originalString.substring(0,2)
val substring2 = originalString.substring(2,5)

println(substring1)
println(substring2)
```
Output: 12
345

## רטרוספקטיבה

מכיוון שמחרוזות הן מבני נתונים נפוצים בכל שפת תכנות, חילוץ מחרוזות הוא כלי חשוב בתחום תכנות. בדוגמאות שלנו עילה נראה כיצד להשתמש בפעולות חילוץ מחרוזת הכוללות פעולות כגון `subSequence()` ו-`substring()`, אשר יכולות להיות מועילות כאשר מעורבים בהפיכת מחרוזות ונתונים.

## ראה גם

- [Kotlin Documentation on subSequence](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-char-sequence/sub-sequence.html)
- [Kotlin Documentation on substring](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/substring.html)