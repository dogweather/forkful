---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
הזמן הנוכחי מסייע למתכנתים ליצור גרסאות, לניתוח נתונים כרוניים ולנטר את ביצועי המערכת. זה מספק מידע נוסף שקשור לנתונים ולביצועים.

## איך לעשות:
השג את התאריך הנוכחי ב-PowerShell באמצעות הפקודה הבאה:

```PowerShell
Get-Date
```
הפלט ייראה כך:

```PowerShell
יום ראשון, 26 בדצמבר 2021 10:39:20
```

## צלילה עמוקה:
בעצם, הפקודה ``Get-Date`` כבר קיימת מאז הגרסא הראשונה של PowerShell. האפשרויות הדינאמיות שלה, תרגומי השפה השונות והתמיכה הרחבה שלה הפכו אותה לברירת מחדל של רבים.

ישנן גם דרכים אחרות להשיג תאריך נוכחי, כמו ``[datetime]::Now`` - אך ``Get-Date`` מומלץ לרוב התרחישים עקב הנוחות שבה.

פירוט על מאחורי הקלעים: PowerShell למעשה מנצל את .NET framework לביצוע של פקודת ``Get-Date``, כאשר הוא מפעיל את המטודה ``System.DateTime.Now``

## ראה גם:
1. [PowerShell Get-Date documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
3. [.NET DateTime class](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)