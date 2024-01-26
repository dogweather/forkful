---
title:                "סידור קוד לתוך פונקציות"
date:                  2024-01-26T01:11:32.532726-07:00
model:                 gpt-4-1106-preview
simple_title:         "סידור קוד לתוך פונקציות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## מה ולמה?
ארגון קוד לתוך פונקציות הוא על קיטוב של נתחי קוד שמבצעים משימות ספציפיות ונתינת שם להם. זה נעשה כדי להפוך קוד לקריא יותר, לניתן לשימוש חוזר ולקל לתחזוקה. במקום לכתוב מחדש את אותו הקוד, קוראים לפונקציה. רוצים לפתור בעיות או לשדרג? מתאימים את הפונקציה בלי לעבור דרך ערימות של סקריפט.

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