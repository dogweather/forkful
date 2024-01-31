---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט ב-PowerShell מאפשרת לשמר מידע בצורה מאורגנת וקלה לגישה. מתכנתים עושים זאת לצורך לוגינג, יצירת קבצי תצורה ושמירת נתונים לשימוש חוזר.

## איך לעשות:
הנה דוגמאות קוד ופלט הדגומה.

### כתיבת טקסט לקובץ:
```PowerShell
$text = "שלום, עולם!"
Out-File -FilePath .\example.txt -InputObject $text -Encoding UTF8
```

### הוספת טקסט לקובץ קיים:
```PowerShell
Add-Content -Path .\example.txt -Value "טקסט נוסף"
```

### כתיבת יותר משורה אחת:
```PowerShell
$lines = "שורה ראשונה", "שורה שנייה", "שורה שלישית"
$lines | Out-File -FilePath .\multipleLines.txt
```

## עיון מעמיק
### היסטוריה:
כתיבה לקבצים הייתה חלק מליבת מערכות ההפעלה מאז ימי המחשבים הראשונים.

### אלטרנטיבות:
בנוסף ל-`Out-File` ו-`Add-Content`, ישנן פונקציות נוספות כמו `Set-Content` ושימוש ב-`.NET` יישומים כמו `[System.IO.File]::WriteAllText()`.

### פרטי יישום:
מומלץ לציין קידוד בעת כתיבת קובץ כדי למנוע בעיות תצוגה, במיוחד בשפות שאינן אנגלית.

## ראו גם
- [Out-File documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)
- [Working with .NET methods](https://docs.microsoft.com/en-us/powershell/scripting/developer/hosting/adding-and-invoking-commands)
