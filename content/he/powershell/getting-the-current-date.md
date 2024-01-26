---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:15:55.045219-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי ב-PowerShell היא פשוט לשלוף את התאריך והשעה בשניה זו. תכניתנים עושים את זה ללוגים, זמנים ותיזמון פעולות.

## איך לעשות:
כדי לקבל את התאריך והשעה הנוכחיים, השתמש בקוד הבא:
```PowerShell
# קבלת התאריך הנוכחי
$התאריך = Get-Date
$התאריך

# פורמט מותאם אישית
$התאריך.ToString("yyyy-MM-dd")
```

דוגמא לפלט:
```
יום רביעי 15 מרץ 2023 10:34:56
2023-03-15
```

## טבילה עמוקה:
הפקודה `Get-Date` זמינה ב-PowerShell מאז הגרסה הראשונה. זו הדרך המובנית והפשוטה ביותר לשליפת תאריך ושעה. אפשרויות חלופיות כוללות שימוש ב-NET בעזרת `[System.DateTime]::Now` או `[System.DateTime]::UtcNow` לזמן אוניברסלי. לגבי פרטים טכניים, `Get-Date` משתמש במערכת ה-API של .NET Framework או .NET Core לקבלת התאריך והשעה תוך שימוש בהגדרות האזור הזמני של המערכת.

## ראה גם:
- [תיעוד הרשמי של `Get-Date`](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [PowerShell ב-GitHub](https://github.com/PowerShell/PowerShell)
