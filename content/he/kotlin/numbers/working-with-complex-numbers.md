---
date: 2024-01-26 04:43:53.241332-07:00
description: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05DE\u05E8\u05D7\u05D9\u05D1\u05D9\u05DD \u05D0\u05EA \u05DE\u05E2\u05E8\
  \u05DB\u05EA \u05D4\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05E9\u05DC\u05E0\u05D5\
  \ \u05DB\u05D3\u05D9 \u05DC\u05DB\u05DC\u05D5\u05DC \u05D0\u05EA \u05E9\u05D5\u05E8\
  \u05E9\u05D9 \u05D4\u05E8\u05D9\u05D1\u05D5\u05E2 \u05E9\u05DC \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05E9\u05DC\u05D9\u05DC\u05D9\u05D9\u05DD, \u05E9\u05DD \u05D4\
  \u05D9\u05D7\u05D9\u05D3\u05D4 '\u05D3\u05DE\u05D9\u05D5\u05E0\u05D9\u05EA' i \u05E9\
  \u05D5\u05D5\u05D4 \u05DC\u05E9\u05D5\u05E8\u05E9 \u05D4\u05E8\u05D9\u05D1\u05D5\
  \u05E2\u05D9 \u05E9\u05DC \u200E-1\u200E. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \u2026"
lastmod: '2024-03-13T22:44:39.261429-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05DE\u05E8\u05D7\u05D9\u05D1\u05D9\u05DD \u05D0\u05EA \u05DE\u05E2\u05E8\
  \u05DB\u05EA \u05D4\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05E9\u05DC\u05E0\u05D5\
  \ \u05DB\u05D3\u05D9 \u05DC\u05DB\u05DC\u05D5\u05DC \u05D0\u05EA \u05E9\u05D5\u05E8\
  \u05E9\u05D9 \u05D4\u05E8\u05D9\u05D1\u05D5\u05E2 \u05E9\u05DC \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05E9\u05DC\u05D9\u05DC\u05D9\u05D9\u05DD, \u05E9\u05DD \u05D4\
  \u05D9\u05D7\u05D9\u05D3\u05D4 '\u05D3\u05DE\u05D9\u05D5\u05E0\u05D9\u05EA' i \u05E9\
  \u05D5\u05D5\u05D4 \u05DC\u05E9\u05D5\u05E8\u05E9 \u05D4\u05E8\u05D9\u05D1\u05D5\
  \u05E2\u05D9 \u05E9\u05DC \u200E-1\u200E. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים מרחיבים את מערכת המספרים שלנו כדי לכלול את שורשי הריבוע של מספרים שליליים, שם היחידה 'דמיונית' i שווה לשורש הריבועי של ‎-1‎. מתכנתים משתמשים בהם בתחומים כמו הנדסה, פיזיקה, ועיבוד אותות, כי הם מעולים במידול גלים, תנודות, וכל דבר שמסתובב.

## איך ל:

בואו נגדיר מחלקה פשוטה למספר מרוכב בKotlin:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // פלט: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // פלט: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // פלט: a * b = (-5.0 + 10.0i)
}
```

## עיון עמוק

מספרים מרוכבים הוזכרו לראשונה במאה ה-16, בפתרון משוואות קוביות שלא היו להן פתרונות ממשיים. הנדסה ופיזיקה נהנות רבות ממספרים מרוכבים לניתוח מעגלי זרם חילופין וגלים. ניתן גם להשתמש בספרייה כמו `koma` או `ejml` של Kotlin לעבודה כבדה יותר.

פעולות על מספרים מרוכבים מחקות את המספרים הממשיים, אבל עם תשומת לב ליחידה הדמיונית. כפל, לדוגמה, נעשה תוך שמירה על התכונה הפילוגית, בזכרון ש-`i^2 = -1‎`. היחידה הדמיונית מאפשרת לנו לייצג מספרים רב-ממדיים, חיוניים בחישובים מדעיים שונים.

## ראו גם

ספריות מתמטיקה של Kotlin:

- [koma](https://koma.kyonifer.com/): ספרייה לחישובים מדעיים עבור Kotlin.

קריאה נוספת על מספרים מרוכבים:

- [ויקיפדיה: מספרים מרוכבים](https://en.wikipedia.org/wiki/Complex_number)
