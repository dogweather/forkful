---
title:                "Kotlin: קריאת קובץ טקסט"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

קריאת קבצי טקסט היא חלק חשוב מתהליך התכנות המודרני. באמצעות קריאת קבצי טקסט, ניתן לטעון ולעבד מידע טקסטואלי בקלות וליישם אותו בתהליך התכנות שלנו.

## איך לעשות זאת

```Kotlin
import java.io.File

fun main() {
    val file = File("sample.txt")
    val lines = file.readLines()
    lines.forEach { line -> println(line) }
}
```

תחילה, ניבנה כיתה בשם "File" עם ערכים של קובץ הטקסט שרוצים לקרוא. לאחר מכן, נשתמש בפונקציית "readLines()" כדי לקרוא את הקובץ ולאחת את השורות שלו לתוך רשימה. לבסוף, נעבור על הרשימה ונדפיס כל שורה בנפרד. 

פלט התוכנית יהיה:

```
This is a sample text file.
It contains multiple lines of text.
We can easily read and process this data with Kotlin.
```

## עיון מעמיק

כדי לקרוא קובץ טקסט בפורמט אחר מUTF-8, ניתן להשתמש בפונקציית "readText(charset: Charset)" עם הפרמטר המתאים של Charset. בנוסף, ניתן לעבור על כל השורות בקובץ באמצעות פונקציית "forEachLine()" במקום "forEach()". כמו כן, ניתן לעבוד עם קובצים גדולים יותר על-ידי שימוש בפונקציית "bufferedReader()" במקום "readLines()".

## ראו גם

- [מדריך לתרגול קוד בכיתה File בקוטלין](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-input-and-output.html)
- [מסמך רשמי על פונקציונליות ועיבוד קבצים בקוטלין](https://kotlinlang.org/docs/functions.html#file-input-and-output)