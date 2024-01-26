---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:50:11.490621-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים זה תהליך שבו המחשב מייצר ערכים באופן שלא ניתן לחזות אותם מראש. תכניתנים משתמשים בזה למגוון מטרות, כולל בדיקות, משחקים ואבטחת מידע.

## איך לעשות:
```PowerShell
# יצירת מספר אקראי בין 1 ל-100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```
פלט לדוגמה: `57`

```PowerShell
# יצירת סדרת מספרים אקראיים
1..10 | ForEach-Object { Get-Random -Minimum 1 -Maximum 101 }
```
פלט לדוגמה: `23 67 45 89 12 37 50 74 91 16`

## טבילה עמוקה
בעבר, כאשר מחשבים עוד לא היו קיימים, מספרים אקראיים היו מופקים באמצעות טבלאות נייר או אמצעים פיזיים אחרים כמו קוביות. היום, אנחנו משתמשים באלגוריתמים כדי לייצר מספרים אקראיים (ולמרבה הפלא, הם לא באמת 'אקראיים' אלא 'פסאודו-אקראיים'). חלופות ל-`Get-Random` כוללות שימוש ב-[RNGCryptoServiceProvider] שמיועד לספק אבטחה גבוהה יותר עם אקראיות טובה יותר. פרטי היישום של יצירת מספרים אקראיים ב-PowerShell מתבססים על .NET ה-[System.Random] קלאס.

## ראה גם
- [מסמך העזרה הרשמי של `Get-Random`](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Get-Random)
- [מידע נוסף על `RNGCryptoServiceProvider`](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider)
