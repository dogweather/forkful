---
title:                "הדפסת פלט לניפוי באגים"
date:                  2024-01-20T17:53:31.172224-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/printing-debug-output.md"
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
