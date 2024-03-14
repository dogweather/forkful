---
date: 2024-01-26 03:45:50.887190-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D4\u05D5\u05D0 \u05D4\u05EA\u05D0\u05DE\u05EA\u05DD \u05DC\u05DE\u05E1\u05E4\
  \u05E8 \u05D4\u05E9\u05DC\u05DD \u05D4\u05E7\u05E8\u05D5\u05D1 \u05D1\u05D9\u05D5\
  \u05EA\u05E8 \u05D0\u05D5 \u05DC\u05D3\u05E8\u05D2\u05EA \u05D3\u05D9\u05D5\u05E7\
  \ \u05E0\u05EA\u05D5\u05E0\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\
  \u05E9\u05E4\u05E8 \u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D4\u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D4\u05E4\u05D7\u05D9\u05EA \u05D0\u05EA\
  \ \u05D3\u05E8\u05D9\u05E9\u05D5\u05EA \u05D4\u05D0\u05D7\u05E1\u05D5\u05DF, \u05D0\
  \u05D5 \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF\u2026"
lastmod: '2024-03-13T22:44:39.263066-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D4\
  \u05D5\u05D0 \u05D4\u05EA\u05D0\u05DE\u05EA\u05DD \u05DC\u05DE\u05E1\u05E4\u05E8\
  \ \u05D4\u05E9\u05DC\u05DD \u05D4\u05E7\u05E8\u05D5\u05D1 \u05D1\u05D9\u05D5\u05EA\
  \u05E8 \u05D0\u05D5 \u05DC\u05D3\u05E8\u05D2\u05EA \u05D3\u05D9\u05D5\u05E7 \u05E0\
  \u05EA\u05D5\u05E0\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E9\
  \u05E4\u05E8 \u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D4\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05DC\u05D4\u05E4\u05D7\u05D9\u05EA \u05D0\u05EA \u05D3\
  \u05E8\u05D9\u05E9\u05D5\u05EA \u05D4\u05D0\u05D7\u05E1\u05D5\u05DF, \u05D0\u05D5\
  \ \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF\u2026"
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
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
