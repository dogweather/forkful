---
title:                "עבודה עם מספרים מרוכבים"
date:                  2024-01-26T04:43:53.241332-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-complex-numbers.md"
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
