---
date: 2024-01-27 20:35:35.356958-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1-PowerShell \u05DE\u05D3\u05D5\
  \u05D1\u05E8\u05EA \u05E2\u05DC \u05D9\u05E6\u05D9\u05E8\u05EA \u05E2\u05E8\u05DB\
  \u05D9\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05D9\u05DD \u05D1\u05DC\u05EA\u05D9\
  \ \u05E6\u05E4\u05D5\u05D9\u05D9\u05DD \u05D1\u05EA\u05D7\u05D5\u05DD \u05DE\u05E1\
  \u05D5\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D9\u05DB\u05D5\u05DC\u05EA \u05D6\u05D5 \u05DE\
  \u05E1\u05D9\u05D1\u05D5\u05EA \u05E8\u05D1\u05D5\u05EA, \u05DB\u05D5\u05DC\u05DC\
  \ \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA, \u05E1\u05D9\u05DE\u05D5\u05DC\u05E6\u05D9\
  \u05D4\u2026"
lastmod: 2024-02-19 22:04:58.943668
model: gpt-4-0125-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1-PowerShell \u05DE\u05D3\u05D5\u05D1\u05E8\
  \u05EA \u05E2\u05DC \u05D9\u05E6\u05D9\u05E8\u05EA \u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05DE\u05E1\u05E4\u05E8\u05D9\u05D9\u05DD \u05D1\u05DC\u05EA\u05D9 \u05E6\u05E4\
  \u05D5\u05D9\u05D9\u05DD \u05D1\u05EA\u05D7\u05D5\u05DD \u05DE\u05E1\u05D5\u05D9\
  \u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05D9\u05DB\u05D5\u05DC\u05EA \u05D6\u05D5 \u05DE\u05E1\u05D9\
  \u05D1\u05D5\u05EA \u05E8\u05D1\u05D5\u05EA, \u05DB\u05D5\u05DC\u05DC \u05D1\u05D3\
  \u05D9\u05E7\u05D5\u05EA, \u05E1\u05D9\u05DE\u05D5\u05DC\u05E6\u05D9\u05D4\u2026"
title: "\u05D2\u05D9\u05DC\u05D5\u05D9 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים ב-PowerShell מדוברת על יצירת ערכים מספריים בלתי צפויים בתחום מסוים. מתכנתים משתמשים ביכולת זו מסיבות רבות, כולל בדיקות, סימולציה ומטרות בטיחותיות, שבהן הבלתי צפויות או החיקוי של אקראיות מהעולם האמיתי היא קריטית.

## איך לעשות:
PowerShell מציע גישה ישירה ליצירת מספרים אקראיים באמצעות ה-cmdlet `Get-Random`. קומנדלט זה יכול לייצר מספרים אקראיים בתחום ברירת המחדל או בתחום מצוין.

```PowerShell
# יצירת מספר אקראי בין 0 לבין Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

כדי לציין טווח, השתמש בפרמטרים `-Minimum` ו`-Maximum`:

```PowerShell
# יצירת מספר אקראי בין 1 ל-100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

לשליטה רבה יותר, ניתן לייצר מופע של הכיתה `System.Random`:

```PowerShell
# שימוש ב-System.Random לסדרה של מספרים
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

אם נדרשת בחירה אקראית ממערך או אוסף, `Get-Random` יכול לבחור פריט באופן ישיר:

```PowerShell
# בחירה אקראית ממערך
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## צלילה עמוקה
ה-cmdlet `Get-Random` ב-PowerShell משתמשת בכיתה של .NET `System.Random` על מנת לייצר מספרים אקראיים פסבדו. הם "פסוֹדו" מכיוון שהם משתמשים באלגוריתמים כדי לייצר רצפים של מספרים שנראים אקראיים בלבד. לרוב היישומים, רמת האקראיות הזו מספקת. עם זאת, למקרים שדורשים אבטחת קריפטוגרפיה, `System.Random` לא מתאימה בגלל טבעה הצפוי.

PowerShell ו־.NET מציעים את `System.Security.Cryptography.RNGCryptoServiceProvider` לאקראיות קריפטוגרפית, שהיא מתאימה יותר ליצירת מפתחות הצפנה או פעולות אחרות שרגישות לאבטחה:

```PowerShell
# מספרים אקראיים בטוחים קריפטוגרפית
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

למרות ש-`Get-Random` ו-`System.Random` מספקים פתרון לצורך רחב באקראיות בכתיבת סקריפטים ולוגיקת יישומים, חשוב לבחור את הכלי הנכון למשימה, במיוחד ביישומים בעלי אוריינטציה אבטחתית שבהם ניבוי יכול להוות פגיעות.
