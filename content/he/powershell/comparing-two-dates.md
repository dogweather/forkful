---
title:                "השוואת שתי תאריכים"
date:                  2024-01-20T17:33:52.411583-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"

category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
שוואת שתי תאריכים היא בדיקה אם תאריך אחד קדם, שוה או מאוחר מהשני. תכנתים עושים זאת כדי לעקוב אחר זמנים, לבצע תיאום משימות, לחשב הפרשי זמן ועוד.

## איך לעשות:
הנה דוגמאות קוד פשוטות שמשוות בין שתי תאריכים ומדפיסות תוצאה:

```PowerShell
$date1 = Get-Date '2023-04-01'
$date2 = Get-Date '2023-04-15'

if ($date1 -eq $date2) {
    "התאריכים זהים"
} elseif ($date1 -lt $date2) {
    "תאריך 1 קודם לתאריך 2"
} else {
    "תאריך 1 מאוחר מתאריך 2"
}
```

תוצאת הדוגמה:
```
תאריך 1 קודם לתאריך 2
```

## עיון עמוק:
שוואת תאריכים היא לב הרבה תוכניות וסקריפטים. בעבר, תכנתים עשו את זה באמצעות מפענחי זמנים ידניים או ספריות שעות ותאריכים. היום, ב-PowerShell, קל מאוד לעשות זאת עם `Get-Date` שמייצר אובייקט DateTime, שאפשר לשוות באמצעות פעולות השוואה (-eq, -lt, -gt וכו'). שימו לב שהשוואה מתחשבת גם באזור הזמן.

## ראה גם:
- [על פקודת Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [על אובייקט DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [תיעוד PowerShell](https://docs.microsoft.com/en-us/powershell/)
