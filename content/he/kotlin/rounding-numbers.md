---
title:                "עיגול מספרים"
date:                  2024-01-26T03:45:50.887190-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

עיגול מספרים הוא התאמתם למספר השלם הקרוב ביותר או לדרגת דיוק נתונה. מתכנתים עושים זאת על מנת לשפר את קריאות הנתונים, להפחית את דרישות האחסון, או מכיוון שהערך המדויק אינו קריטי לחישובים הבאים.

## איך לעשות:

בKotlin, ניתן לבצע עיגול באמצעות מספר פונקציות כמו `roundToInt()`, `roundToDouble()`, ובאמצעות `BigDecimal` לשליטה רבה יותר:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // פלט: 3

    val number2 = 3.5
    println(number2.roundToInt()) // פלט: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // פלט: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // פלט: 123.5
}
```

## חקירה עמוקה

בהיסטוריה, עיגול מספרים היה מושג יסוד במתמטיקה ובחישוב, שמטרתו להתמודד עם מגבלות הדיוק המספרי. בשנים הראשונות של החישוב, עיגול היה מכריע עקב עלות הזכרון הגבוהה.

בKotlin, העיגול מבוסס על ספריות ג'אווה הסטנדרטיות. אפשרויות לעיגול כוללות `Math.round()`, שעוגל למספר השלם הקרוב ביותר, ו`BigDecimal` לעיגול מותאם אישית, שם אפשר לציין סקאלה ו`RoundingMode`.

לכל `RoundingMode` יש מדיניות שונה להתמודדות עם תיקויות (כאשר הספרה נמצאת בדיוק באמצע האפשרויות לעיגול). לדוגמא, `RoundingMode.HALF_UP` עוגל לשכן הקרוב ביותר, אלא אם שני השכנים שווים מרחק, במקרה זה הוא יעגל למעלה.

## ראה גם

- תיעוד Kotlin על [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- תיעוד Java של Oracle לגבי [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- התקן IEEE לחישוב נקודה צפה (IEEE 754) [IEEE Standard 754](https://ieeexplore.ieee.org/document/4610935)
