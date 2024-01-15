---
title:                "יצירת קובץ זמני"
html_title:           "Kotlin: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

למה ייתכן שמישהו ירצה ליצור קובץ זמני בקוד שלו? בעיקר כאמצעי זמני לשמור מידע ולשתף בנתונים ללא להתקין על המערכת המרכזית. 

## כיצד ליצור קובץ זמני ב-Kotlin

בשפת קוטלין, ישנם שני צורות ליצור קבצים זמניים - באמצעות הפונקציה `createTempFile ()` או על ידי יצירת אובייקט מסוג `File` והשתמש בפונקציה `deleteOnExit ()`. נבחר להשתמש בפונקציה הראשונה. למדה קוד נמעט:

```Kotlin
val tempFile = createTempFile("temp", ".txt")  // יוצר קובץ זמני בשם "temp" עם הסיומת ".txt"
tempFile.writeText("זוהי תוכן זמני שנשמר בקובץ")  // כותב תוכן לקובץ
println(tempFile.readText())  // קורא את התוכן מהקובץ

// עכשיו נגדיל את הקובץ
val enlargedFile = tempFile.sized(1024 * 1024)  // נהפוך את הקובץ לגודל של 1 מגהבייט על ידי השתמשות בפונקציה מובנית 
println(enlargedFile.length())  // אורך הקובץ צריך להיות 1 מגהבייט
```

כאן נשתמש בפונקציה `createTempFile()` כדי ליצור קובץ זמני עם שם וסיומת מסוימים. אחר כך, נשתמש בפונקציות `writeText()` ו- `readText()` כדי לכתוב ולקרוא תוכן לקובץ. נמצא גם שימוש בפונקציה `sized()` כדי לשנות את גודל הקובץ ל- 1 מגהבייט. סיום הקוד אמור להדפיס את האורך הנכון של הקובץ.

## Deep Dive

בעבודה עם קבצים זמניים כדאי לזכור כמה נקודות חשובות. ראשית, על קבצים זמניים לא סתם פספוסים - הם מאוחסנים בתיקיית ה- system ואינ