---
title:    "Kotlin: כתיבה לתקליטן התקנה"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

לכתוב לתקליט השגיאות (standard error) הוא כלי חשוב בתוכנות בKotlin. התקליט השגיאות הוא מקום שבו תוכניות מדווחות על שגיאות שקרו בזמן הריצה, וכן ניתן להשתמש בו לכתוב הודעות למשתמש כאשר מחזירים תוצאות לא צפויות.

## איך לעשות זאת

כתיבת הודעות לתקליט השגיאות בKotlin נעשה באמצעות הפונקציה "println()". ניתן להשתמש בהפונקציה על מנת להדפיס תוצאות או הודעות לתקליט השגיאות כדלקמן:

```Kotlin
fun main() {
  println("הודעת שגיאה לתקליט השגיאות")
}
```

פלט:

"הודעת שגיאה לתקליט השגיאות"

בנוסף, ניתן להשתמש בפונקציה "System.err.println()" כדי להדפיס הודעות מדווחות על שגיאות שקרו בתוכנית.

## צלילה עמוקה

העברת הודעות לתקליט השגיאות הוא תהליך חיוני בכתיבת תוכניות בKotlin. ניתן להשתמש בזה באופן יעיל כדי לזהות ולנתח שגיאות כאשר התוכנית מורצת. כמו כן, ניתן להשתמש בתקליט השגיאות להדפיס הודעות מתאימות למשתמש כאשר מחזירים תוצאות לא צפויות.

## ראה גם

- פונקציית "println" בדוקומנטציית Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/println.html
- פונקציית "System.err.println" בדוקומנטציית Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io/-print-writer/println.html