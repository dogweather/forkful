---
title:                "Kotlin: מציאת אורך של מחרוזת"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה

למה יש לאדם להתעסק במציאת אורך של מחרוזת? מציאת אורך של מחרוזת חיוני ביותר לפתרון בעיות תכנותיות שונות. כאשר אנחנו עולים על פינג פונג של שרשרת תווים, חשוב לנו לדעת את האורך של השרשרת כדי לכוון את הפעולות התכנותיות המתאימות.

## איך לעשות זאת

על מנת למצוא את האורך של מחרוזת בקוד קוטלין, ניתן להשתמש בפונקציית "length". לדוגמה:

```Kotlin
var string = "כתוב blog post על תכנות בקוטלין"
var length = string.length

println("אורך המחרוזת הוא $length") // הופקד לסך של 33
```

## חקירה מעמיקה

כעת נתאר מספר פעולות נוספות שניתן לבצע על מחרוזות בקוטלין:

- ניתן להשתמש בפונקציית "substring" כדי לקבל חלק מסוים של מחרוזת, על ידי ציון אינדקס התחלה ואינדקס סיום.

```Kotlin
var string = "Hello World"
var subString = string.substring(0, 5)

println(subString) // הופקד "Hello"
```

- ניתן לשנות את ערכי התווים במחרוזת באמצעות הפונקצייה "replace". לדוגמה, נשנה את התו "l" לתו "r".

```Kotlin
var string = "Hello World"
var newString = string.replace("l", "r")

println(newString) // הופקד "Herr World"
```

- ניתן להמיר את המחרוזת למערך של תווים באמצעות הפונקצייה "toCharArray".

```Kotlin
var string = "Hello World"
var charArray = string.toCharArray()

println(charArray) // הופקד מערך התווים ['H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd']
```

## ראה גם

- [פונקציות מובנות בקוטלין](https://www.geeksforgeeks.org/kotlin-built-in-functions/)
- [מדריך לתכנות בקוטלין](https://www.tutorialspoint.com/kotlin/index.htm)
- [מאמר על פונקציית "length" בקוטלין](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/length.html)