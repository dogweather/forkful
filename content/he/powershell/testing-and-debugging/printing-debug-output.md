---
date: 2024-01-20 17:53:31.172224-07:00
description: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\
  \u05E4\u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D4\u05D9\u05D0 \u05E1\u05EA\
  \u05DD \u05D3\u05E8\u05DA \u05DC\u05D4\u05E6\u05D9\u05D2 \u05DE\u05D9\u05D3\u05E2\
  \ \u05D1\u05DE\u05D4\u05DC\u05DA \u05D4\u05E8\u05E6\u05EA \u05D4\u05E7\u05D5\u05D3\
  , \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9 \u05D4\
  \u05EA\u05E0\u05D4\u05DC\u05D5\u05EA\u05D5. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D0\u05D1\u05D7\u05DF \u05D1\u05E2\u05D9\u05D5\u05EA \u05D5\
  \u05DC\u05D4\u05D1\u05D9\u05DF \u05D8\u05D5\u05D1 \u05D9\u05D5\u05EA\u05E8 \u05DE\
  \u05D4 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.703048-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D4\u05D9\u05D0 \u05E1\u05EA\u05DD\
  \ \u05D3\u05E8\u05DA \u05DC\u05D4\u05E6\u05D9\u05D2 \u05DE\u05D9\u05D3\u05E2 \u05D1\
  \u05DE\u05D4\u05DC\u05DA \u05D4\u05E8\u05E6\u05EA \u05D4\u05E7\u05D5\u05D3, \u05DB\
  \u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9 \u05D4\u05EA\
  \u05E0\u05D4\u05DC\u05D5\u05EA\u05D5. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05D0\u05D1\u05D7\u05DF \u05D1\u05E2\u05D9\u05D5\u05EA \u05D5\u05DC\
  \u05D4\u05D1\u05D9\u05DF \u05D8\u05D5\u05D1 \u05D9\u05D5\u05EA\u05E8 \u05DE\u05D4\
  \ \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD\u2026"
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלט לניפוי באגים היא סתם דרך להציג מידע במהלך הרצת הקוד, כדי לעקוב אחרי התנהלותו. תוכניתנים עושים את זה כדי לאבחן בעיות ולהבין טוב יותר מה הקוד שלהם עושה בזמן אמת.

## איך לעשות:
כאשר אתה רוצה להדפיס הודעות ניפוי ב-PowerShell, תוכל להשתמש בפקודה `Write-Host` או `Write-Debug`, בהתאם לצורך שלך. נתחיל בדוגמא פשוטה עם `Write-Host`:

```PowerShell
# הדפסת הודעה פשוטה
Write-Host "שלום, זהו פלט דיבאג!"

# הדפסה עם צבע
Write-Host "זהות עערורית!" -ForegroundColor Red
```

כעת עבור `Write-Debug`:

```PowerShell
# הגדרת משתנה עם אמת מידה ברירת מחדל להודעות ניפוי
$DebugPreference = 'Continue'

# הדפסת הודעת ניפוי
Write-Debug "עכשיו אני נמצא פה!"
```

פלט לדוגמה:

```
שלום, זהו פלט דיבאג!
זהות עערורית!
DEBUG: עכשיו אני נמצא פה!
```

## נפלה לעומק:
בעבר, הדפסה לקונסול הייתה הדרך העיקרית לניפוי באגים. ב-PowerShell, `Write-Host` הוא ישיר ומראה פלט בקונסול, אבל לא משפיע על זרימת נתונים בפועל. בניגוד אליו, `Write-Debug` משמש להדפסת הודעות ניפוי שניתן לשלוט בהן בעזרת המשתנה `$DebugPreference`. זה מאפשר יותר גמישות במהלך הפיתוח וניתוח תקלות כאשר תוכנה כבר בשימוש בסביבות ייצור. יש גם פקודות אחרות כמו `Write-Verbose` ו-`Write-Information` שיכולות לעזור, תלוי בסוג המידע שברצונך לרשום.

## ראה גם:
עיין במקורות הבאים למידע נוסף:

- [about_Write-Host](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-host)
- [about_Preference_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_preference_variables)
