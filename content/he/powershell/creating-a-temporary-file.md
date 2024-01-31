---
title:                "יצירת קובץ זמני"
date:                  2024-01-20T17:41:15.415309-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא תהליך שבו אנו מייצרים קובץ שמיועד לשימוש חד-פעמי או קצר-טווח. תכניתנים עושים זאת כדי לאחסן מידע זמני, כגון נתונים לפני עיבוד, או כחלק מתהליך דיבאג.

## איך לעשות:
כדי ליצור קובץ זמני ב-PowerShell, ניתן להשתמש בפונקציה `New-TemporaryFile`:

```PowerShell
# יצירת קובץ זמני
$tempFile = New-TemporaryFile
# הדפסת שם הקובץ
Write-Host "Temporary file created: $($tempFile.FullName)"
```

תוצאת דוגמה:
```
Temporary file created: C:\Users\<UserName>\AppData\Local\Temp\tmpA1B2.tmp
```

## צלילה לעומק:
בעבר, יצירת קובץ זמני נעשתה על ידי קביעת שם קובץ באופן ידני ושמירת קובץ עם סיומת זמנית. הפונקציה `New-TemporaryFile` מיושמת מאז PowerShell 5.0 ומקלה על התהליך על ידי אוטומציה. ישנן אלטרנטיבות כמו שימוש ב-[System.IO.Path]::GetTempFileName() המובנית של .NET ב-PowerShell:

```PowerShell
# אלטרנטיבה ליצירת קובץ זמני עם .NET
$tempFileName = [System.IO.Path]::GetTempFileName()
Write-Host "Temporary file created: $tempFileName"
```

זכרו, קבצים זמניים נועדו להימחק אחרי שימוש. אם לא תנקו אותם, הם יכולים לצבור ולכבד את המערכת.

## גם כדאי לראות:
- [מדריך לפונקציה New-TemporaryFile](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7)
