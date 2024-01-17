---
title:                "יצירת קובץ זמני"
html_title:           "Java: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא תהליך שמשמש ליצירת קובץ שימושי מקומי שיימחק בסופו של דבר. תהליך זה נעשה כדי לקבל גישה מיידית לקובץ זמני וכדי לאחסן נתונים באופן מתאים זמנית, תוך שימוש בשפת תכנות הג'אווה.

## איך לעשות:
כאשר משתמשים בשפת תכנות הג'אווה, ניתן ליצור קובץ זמני באמצעות היצירה של אובייקט "File" ומתן לו שם או סימן ייחודי באמצעות הפונקציה "createTempFile". לדוגמה:

```Java
// יצירת קובץ זמני בכתיבת "temp"
File tempFile = File.createTempFile("temp", ".txt");
```

ניתן להשתמש גם בפונקציה "createTempFile" כדי ליצור קובץ זמני במיקום מסוים, כך שהקובץ ייווצר כחלק מהדרגת התת-קיבץ. לדוגמה:

```Java
// יצירת קובץ זמני בתת-קיבץ של קובץ "temp"
File tempFile = File.createTempFile("temp", ".txt", new File("C:/myDir/tempFiles"));
```

על מנת למחוק את הקובץ זמני, נוכל להשתמש בפונקציה "delete" של אובייקט הקובץ. לדוגמה:

```Java
// מחיקת הקובץ זמני
tempFile.delete();
```

## התעמולה לעומק:
יצירת קובץ זמני היא תהליך נפוץ וחשוב בתכנות. תהליך זה עוזר למתכנתים ליצור ולאחסן נתונים באופן מתאים זמנית עד שהם יוכלו להיות שלחו וליצור את הקובץ הסופי. בנוסף, יצירת קובץ זמני עוזרת למתכנתים לבדוק פעולות ולשלוח נתונים לתוך קובץ לפני הכתיבה אליו.

אתרים למידע נוסף:

- תיעוד רשמי של המחלקה File: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- הסבר על פונקציות נוספות של יצירת קבצים זמניים: https://www.geeksforgeeks.org/file-class-in-java/#createTempFile() 

## ראה גם:
https://www.baeldung.com/java-temporary-files - מאמר מפורט יותר על יצירת קבצים זמניים בשפת הג'אווה.