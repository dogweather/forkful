---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט היא פריט בסיסי בתכנות שמאפשר לנו לקחת מקובץ טקסט קיים נתונים. תכניתים מבצעים זאת במגוון רחב של תרחישים, כמו ניתוח קבצי לוג, קריאה של נתונים לעיבוד, ועוד.

## איך לעשות?

על מנת לקרוא קובץ טקסט באמצעות PowerShell, נוכל להשתמש בפקודה `Get-Content`:

```PowerShell
Get-Content -Path C:\temp\yourfile.txt
```
אתה תראה את המידע שהתקבל מן הקובץ כפלט ישיר במסוף.

## הצלילה הרחוקה

הפקודה `Get-Content` היא חלק מ-PowerShell מאז ההתחלה, מה שהופך אותה לכלי נוסף ואמין. יתר על כן, ישנם גם דרכים חליפיות לקרוא קובץ טקסט ב-PowerShell, כמו למשל `[System.IO.File]::ReadAllText()` או `[System.IO.File]::ReadAllLines()`. פרטי המימוש של קריאת קובץ טקסט משתנים בהתאם לרמת הפונקציונאליות של הקובץ, כמו גם לצורך הספציפי.

## ראה גם

ודא שאתה מבדוק קישורים נוספים שיעזרו לך להבין את המשמעות המלאה של קריאת קובץ טקסט ב-PowerShell:

- הבנה עמיקה יותר של פקודת Get-Content: 
    - https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1

- איך לקרוא קובץ טקסט באמצעות [System.IO.File]::ReadAllText():
    - https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext?view=net-5.0

- איך לקרוא קובץ טקסט באמצעות [System.IO.File]::ReadAllLines():
    - https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalllines?view=net-5.0