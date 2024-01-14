---
title:    "Kotlin: יצירת מספרים אקראיים"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why (למה):
כתיבת קוד ופיתוח יישומים היא תחום מאתגר ומרתק עבור רבים, וגנרציה של מספרים אקראיים היא חלק חשוב מתהליך התכנות. גנרציה של מספרים אקראיים מאפשרת לנו ליצור יישומים שיכולים לתפקד בצורה שונה כל פעם שהם מופעלים.

## How To (כיצד לבצע):
בהתחלה, אנו נצרף לספריית הוברט המסופקת על ידי קודונג. אחר כך, נשתמש בפונקציה "nextInt()" כדי לקבל מספר אקראי בין 1 ל-10 ונדפיס אותו למסך בעזרת הפונקציה "println()". באמצעות לולאה forEach, נעשה כמה נסיונות ונדפיס את התוצאות למסך.

```Kotlin
import java.util.*

fun main() {
    val random = Random()
    println("Generating 10 random numbers:")
    
    // Using a forEach loop to generate 10 random numbers and print them
    (1..10).forEach {
        val randomNumber = random.nextInt(10) + 1
        println(randomNumber)
    }
}
```
פלט:
```
Generating 10 random numbers:
7
2
9
8
5
10
4
3
1
6
```

## Deep Dive (טיפול מעמיק):
גנרציה אקראיית של מספרים מתבצעת על ידי יצירת מספרים באופן מאוד מסודר ומתוכנת, והם נראים כאילו הם נבחרים בצורה אקראית. אבל בעצם, המחשב משתמש באלגוריתם מסוים המשתמש במידע פנימי כדי ליצור מספרים לא מקבילים.

## See Also (ראה גם):
- [ספריית הוברט ב-Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)
- [ישות Random ב-Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [מאמר על גנרציית מספרים אקראיים](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)