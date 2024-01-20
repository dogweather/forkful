---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירה של קובץ זמני היא בדיוק כמו שזה נשמע. זהו קובץ שמתווסף באופן זמני למערכת הקבצים שלך. המתכנתים עושים זאת כדי לאחסן נתונים זמניים שאינם נדרשים לאחר שהתהליך (או העבודה) נותרת מעבר למדי.  

## כיצד ל:
דוגמאות לקוד ופלט דוגמתי בתוך ```Kotlin ... ``` מקטעי קוד.
```Kotlin
import java.nio.file.Files

fun main() {
      // Creating a temporary file
      val tempFile = Files.createTempFile("tempFile", ".txt")
      
      println("Temporary file created: ${tempFile.toAbsolutePath()}")
}
```
פלט דוגמתי:
```
Temporary file created: C:\Users\...\AppData\Local\Temp\tempFile5874387397786809928.txt
```

## בהיקף עמוק
קובצים זמניים משמשים בדרך כלל לאחסון נתונים זמניים או לצורף ערוצים לעבודה. בהיסטוריה, קובצים זמניים היו זמינים בעיקר בליבת המערכת, אך הם התרבו בעזרת שפות תכנות מודרניות שמאפשרות יצירה נוחה יותר. כמובן, ניתן לבחור גם בגישות אחרות, כמו השימוש במסד הנתונים, אך הם יכולים להיות מסובכים יותר מנקודת מבט של ביצועים וניהול משאבים.
במהלך ביצוע הקוד, שם הקובץ הזמני שמתווסף שונה באופן רנדומלי כדי למנוע התנגשות משם קובץ.

## ראה גם:
- [Oracle - Creating, Reading, and Writing Files](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)