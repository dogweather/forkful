---
title:    "Kotlin: כתיבה אל תוך השגיאה התקנית"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## למה

כתיבה לפלט השגיאה התקני היא פעולה חשובה בתכנות ב-Kotlin. היא מאפשרת למתכנת לראות את הודעות השגיאה של התוכנית שלו ולאפס אותם. כלומר, זה נחישות וזה עוזר לפתור בעיות בקוד.

## איך לעשות זאת

לכתוב לפלט השגיאה התקני ב-Kotlin, ניתן להשתמש בפונקציית "System.err.println()" ולמסור לה את התוכן של הודעת השגיאה כפרמטר. ניתן גם להשתמש בפונקציות נוספות כמו "System.err.print()" ו-"System.err.write()" לכתוב לפלט השגיאה בצורה שונה.

```Kotlin
fun main(args: Array<String>) {
  val num1 = 10
  val num2 = 0
  try {
    val result = num1 / num2
    System.err.println("התוצאה היא $result")
  } catch (e: Exception) {
    System.err.println("אירעה שגיאה: ${e.message}")
  }
}
```

פלט:

```
אירעה שגיאה: / by zero
```

## מעמק

בנוסף לכתיבה לפלט השגיאה התקני, ניתן גם למצוא את תמתינת השגיאה באמצעות המחלקה "System.err". המחלקה מכילה פונקציות נוספות כמו "System.err.flush()" שמנקה את הפלט, "System.err.checkError()" שבודקת אם יש שגיאה ו-"System.err.close()" שסוגרת את הפלט.

## ראו גם

- [כתיבה לפלט השגיאה התקני ב-Kotlin](https://kotlinlang.org/docs/tutorials/command-line.html)
- [משנתו של System.err](https://docs.oracle.com/javase/10/docs/api/java/lang/System.html#err)