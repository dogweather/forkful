---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:07.757995-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05EA\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8 (stderr) \u05D1-PowerShell \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05E9\u05DC\u05D9\u05D7\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA\
  \ \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D0\u05D5 \u05D3\u05D9\u05D0\u05D2\u05E0\u05D5\
  \u05E1\u05D8\u05D9\u05E7\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC\u05D6\
  \u05E8\u05DD stderr, \u05E9\u05D5\u05E0\u05D4 \u05DE\u05D6\u05E8\u05DD \u05D4\u05E4\
  \u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout). \u05D4\u05D4\
  \u05E4\u05E8\u05D3\u05D4 \u05D4\u05D6\u05D5 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \u2026"
lastmod: '2024-02-25T18:49:37.964932-07:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05EA \u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8 (stderr) \u05D1-PowerShell \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E9\u05DC\u05D9\u05D7\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\
  \u05D2\u05D9\u05D0\u05D4 \u05D0\u05D5 \u05D3\u05D9\u05D0\u05D2\u05E0\u05D5\u05E1\
  \u05D8\u05D9\u05E7\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC\u05D6\u05E8\
  \u05DD stderr, \u05E9\u05D5\u05E0\u05D4 \u05DE\u05D6\u05E8\u05DD \u05D4\u05E4\u05DC\
  \u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout). \u05D4\u05D4\u05E4\
  \u05E8\u05D3\u05D4 \u05D4\u05D6\u05D5 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה לשגיאת סטנדרט (stderr) ב-PowerShell כוללת שליחת הודעות שגיאה או דיאגנוסטיקה ישירות לזרם stderr, שונה מזרם הפלט הסטנדרטי (stdout). ההפרדה הזו מאפשרת שליטה מדויקת יותר בפלט של סקריפט, מה שמאפשר למפתחים להכווין הודעות רגילות והודעות שגיאה ליעדים שונים, הכרחי לטיפול בשגיאות ולוגינג.

## איך לעשות:

PowerShell מפשטת את התהליך של כתיבה ל-stderr באמצעות שימוש ב-cmdlet `Write-Error` או על ידי הכוונת פלט לשיטה `$host.ui.WriteErrorLine()`. עם זאת, לניתוב stderr ישיר, ייתכן שתעדיף שימוש בשיטות .NET או בניתוב מזהה קובץ ש-PowerShell עצמו מציע.

**דוגמה 1:** שימוש ב-`Write-Error`לכתיבת הודעת שגיאה ל-stderr.

```powershell
Write-Error "This is an error message."
```

פלט ל-stderr:
```
Write-Error: This is an error message.
```

**דוגמה 2:** שימוש ב-`$host.ui.WriteErrorLine()` לכתיבת stderr ישירה.

```powershell
$host.ui.WriteErrorLine("Direct stderr write.")
```

פלט ל-stderr:
```
Direct stderr write.
```

**דוגמה 3:** שימוש בשיטות .NET לכתיבה ל-stderr.

```powershell
[Console]::Error.WriteLine("Using .NET method for stderr")
```

פלט של שיטה זו:
```
Using .NET method for stderr
```

**דוגמה 4:** ניתוב פלט שגיאה באמצעות מזהה קובץ `2>`.

מזהי קובצים ב-PowerShell יכולים לנתב זרמים שונים. עבור stderr, מזהה הקובץ הוא `2`.הנה דוגמה לניתוב stderr לקובץ בשם `error.log` בעת ביצוע פקודה שיוצרת שגיאה.

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

דוגמה זו לא מייצרת פלט לקונסול, אבל יוצרת קובץ `error.log` בתיקייה הנוכחית המכילה את הודעת השגיאה מנסיון הגישה לקובץ שלא קיים.

כמסקנה, PowerShell מספקת מגוון שיטות לכתיבה וניהול יעיל של פלט שגיאה, מה שמאפשר אסטרטגיות מתקדמות לטיפול בשגיאות ולוגינג בסקריפטים וביישומים.
