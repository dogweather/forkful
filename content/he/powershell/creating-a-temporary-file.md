---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא פעולה שבה נוצר קובץ מבין הרעיון באמצעות קוד תכנות. תכנתים משתמשים בקבצים אלו לאחסון מניפולציה זמנית של נתונים שלאחר מכן מתמחקים.

## איך:
```PowerShell
# הקוד הבא יוצר קובץ זמני
$tmpFile = [System.IO.Path]::GetTempFileName()
```
זה יחזיר:
```
C:\Users\USERNAME\AppData\Local\Temp\tmp4D93.tmp
```
## Deep Dive
היסטורית, הקבצים הזמניים היו אמצעי נפוץ לאחסון מניפולצייה של נתונים לצורך פעילויות אשר דרשו שמירת מצב זמני. למרות זאת, בימים אלה ישנן אלטרנטיבות טובות יותר לקבצים זמניים, כמו השמירה על נתונים במטמון או בענן.
ב-PowerShell, קבצים זמניים נוצרים באמצעות המודול `[System.IO.Path]::GetTempFileName()` שגורם ליצירת קובץ חדש עם שם ייחודי בדירקטוריונים שה'OS' קובע כ'זמני'.

## ראה גם
- [PowerShell: הגדרת קבצים זמניים](https://www.powershellmagazine.com/2014/07/18/pstip-how-to-create-a-temporary-file-with-powershell/)
- [מדריך למפתחים של Microsoft לקבצים זמניים](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)