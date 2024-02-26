---
date: 2024-01-20 17:33:52.411583-07:00
description: "\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D4\u05D9\u05D0 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\
  \u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\u05D7\u05D3 \u05E7\u05D3\u05DD, \u05E9\
  \u05D5\u05D4 \u05D0\u05D5 \u05DE\u05D0\u05D5\u05D7\u05E8 \u05DE\u05D4\u05E9\u05E0\
  \u05D9. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\
  \ \u05D6\u05DE\u05E0\u05D9\u05DD, \u05DC\u05D1\u05E6\u05E2 \u05EA\u05D9\u05D0\u05D5\
  \u05DD \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA, \u05DC\u05D7\u05E9\u05D1 \u05D4\u05E4\
  \u05E8\u05E9\u05D9 \u05D6\u05DE\u05DF \u05D5\u05E2\u05D5\u05D3."
lastmod: '2024-02-25T18:49:37.958466-07:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05D4\u05D9\u05D0 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD\
  \ \u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\u05D7\u05D3 \u05E7\u05D3\u05DD, \u05E9\u05D5\
  \u05D4 \u05D0\u05D5 \u05DE\u05D0\u05D5\u05D7\u05E8 \u05DE\u05D4\u05E9\u05E0\u05D9\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05D6\
  \u05DE\u05E0\u05D9\u05DD, \u05DC\u05D1\u05E6\u05E2 \u05EA\u05D9\u05D0\u05D5\u05DD\
  \ \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA, \u05DC\u05D7\u05E9\u05D1 \u05D4\u05E4\u05E8\
  \u05E9\u05D9 \u05D6\u05DE\u05DF \u05D5\u05E2\u05D5\u05D3."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
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
