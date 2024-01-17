---
title:                "כתיבה לשגיאת מערכת תקן"
html_title:           "Kotlin: כתיבה לשגיאת מערכת תקן"
simple_title:         "כתיבה לשגיאת מערכת תקן"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# About Writing to Standard Error in Kotlin

## מה ולמה?

כתיבה לפלט השגאה (standard error) הוא פשוט הדפסת הודעות שגיאה לטרמינל. למרות שזה נראה כמו משהו פשוט, כתיבה לפלט השגאה הוא מאוד חשוב לתכנותנים. כשבקוד שלנו יש שגיאות, עלינו להתקנן על התקלה כמה שיותר מהר בכדי להפוך לקוד יעיל ותקין. כתיבה לפלט השגאה מאפשרת לנו לראות מה כל הטרמינל מציג כשהמשתמש מריץ את הקוד שלנו, וכך אנחנו יכולים לעקוב אחר השגיאות שלנו ולתקן אותם.

## איך לכתוב:

כדי להדגים את כתיבת השגיאות בקוד Kotlin, ניצור שני דוגמאות. הקוד ישתמש בפקודה `println` כדי להדפיס את ההודעה לפלט השגאה. נתחיל עם פונקציה פשוטה שמדפיסה את השם של השפה:

```kotlin
fun main() {
    println("Kotlin")
}
```

כשנריץ את הקוד הזה במחברת Kotlin, נקבל את הפלט הבא בטרמינל:

```
Kotlin
```

עכשיו, נוסיף טקסט למתודה `main` שיכול לגרום לטעות במקרה של אינדקסים שלא קיימים במערך:

```kotlin
fun main() {
    println("Kotlin")
    val nums = arrayOf(1, 2, 3)
    println(nums[3])
}
```

כאן נקבל כמובן את השגיאה "ArrayIndexOutOfBoundsException" שתופיע בפלט השגיאה של הטרמינל:

```
Kotlin
Exception in thread "main" java.lang.ArrayIndexOutOfBoundsException: Index 3 out of bounds for length 3
	at MainKt.main(Main.kt:4)
```

## מעמקים:

היסטורית השגיאות נכתב לטרמינל כבר בשנות ה-70 של המאה ה-20, כשאנשי מעבדות Unix רצו לצפות בהודעות שגיאה בתוך הטקסט של התכנית שהם כתבו. סיבה נוספת שבגללה כתיבה לפלט השגאה חשובה היא כי היא מאפשרת לנו לראות את התהליך של הקוד כאשר הוא מתבצע, ולכן היא עוזרת לנו לזהות בעיות ולתקן אותן כמה שיותר מהר.

בנוסף לפקודה `println` שהשתמשנו בה בדוגמאות הקוד שלנו, ישנם גם פקודות נוספות כמו `print` ו- `System.err` שיכולות לשמש כדי להדפיס הודעות שגיאה לפלט השגאה.

## ראו גם:

למידע נוסף על כתיבה לפלט השגאה וכיצד להשתמש בה בקוד שלכם, ניתן להעיין במקורות הבאים:

- [Kotlin Language Documentation](https://kotlinlang.org/docs/reference/basic-types.html#exceptions)
- [Java Tutorials: Writing to Standard Error](https://docs.oracle.com/javase/tutorial/essential/io/formatting.html)