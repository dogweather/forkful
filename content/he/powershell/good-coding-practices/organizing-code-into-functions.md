---
date: 2024-01-26 01:11:32.532726-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D5\u05D0\
  \u05D5 \u05E0\u05DB\u05EA\u05D5\u05D1 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ \u05E9\u05EA\u05D7\u05E9\u05D1 \u05D0\u05EA \u05E1\u05DB\u05D5\u05DD \u05E9\u05DC\
  \ \u05E9\u05E0\u05D9 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD. \u05E4\u05E9\u05D5\u05D8\
  , \u05D0\u05D1\u05DC \u05D6\u05D4 \u05DE\u05DE\u05D7\u05D9\u05E9 \u05D0\u05EA \u05D4\
  \u05E0\u05E7\u05D5\u05D3\u05D4."
lastmod: '2024-03-13T22:44:39.708207-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D5\u05D0\u05D5 \u05E0\u05DB\u05EA\u05D5\u05D1 \u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D4 \u05E9\u05EA\u05D7\u05E9\u05D1 \u05D0\u05EA \u05E1\u05DB\
  \u05D5\u05DD \u05E9\u05DC \u05E9\u05E0\u05D9 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  ."
title: "\u05E1\u05D9\u05D3\u05D5\u05E8 \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
weight: 18
---

## איך לעשות:
בואו נכתוב פונקציה שתחשב את סכום של שני מספרים. פשוט, אבל זה ממחיש את הנקודה.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# קריאה לפונקציה עם 5 ו-10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "הסכום הוא $sum"
```

פלט לדוגמא:

```
הסכום הוא 15
```

## צלילה לעומק
פונקציות ב-PowerShell, כמו ברוב השפות, הן חדשות ישנות. אנחנו מחלקים קוד למקטעים מימים של Fortran. זה עוסק ב'לא להמציא את הגלגל מחדש'. חלופות? בטח, סקריפטים או cmdlets. אך להם חסרה האסתטיקה והרגישות להקשר של פונקציות בתוך סקריפטים.

יישום? פונקציות יכולות להיות בסיסיות כמו הדוגמא שלנו או מורכבות עם scope-ים, קלט מה-pipeline, ועוד. לקחו לדוגמא `פונקציות מתקדמות`. הן מחקות cmdlets עם פרמטרים שיש להם אטריביוטים, כמו `[Parameter(Mandatory=$true)]`. זו טעימה מהגמישות של PowerShell.

## ראו גם
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
