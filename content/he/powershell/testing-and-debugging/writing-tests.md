---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:06.163093-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1-PowerShell \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05E9\u05DE\u05D0\u05DE\u05EA\
  \u05D9\u05DD \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05EA \u05D0\u05EA \u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05E7\u05D5\
  \u05D3 \u05E9\u05DC\u05DA \u05D1-PowerShell, \u05D5\u05DE\u05D1\u05D8\u05D9\u05D7\
  \u05D9\u05DD \u05E9\u05D4\u05D5\u05D0 \u05DE\u05EA\u05E0\u05D4\u05D2 \u05DB\u05E4\
  \u05D9 \u05E9\u05E6\u05E4\u05D5\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.704675-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  -PowerShell \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\
  \u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05E9\u05DE\u05D0\u05DE\u05EA\u05D9\u05DD\
  \ \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05EA \u05D0\u05EA \u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05E7\u05D5\u05D3\
  \ \u05E9\u05DC\u05DA \u05D1-PowerShell, \u05D5\u05DE\u05D1\u05D8\u05D9\u05D7\u05D9\
  \u05DD \u05E9\u05D4\u05D5\u05D0 \u05DE\u05EA\u05E0\u05D4\u05D2 \u05DB\u05E4\u05D9\
  \ \u05E9\u05E6\u05E4\u05D5\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## מה ולמה?

כתיבת בדיקות ב-PowerShell כוללת יצירת סקריפטים שמאמתים אוטומטית את פונקציונליות הקוד שלך ב-PowerShell, ומבטיחים שהוא מתנהג כפי שצפוי. מתכנתים עושים זאת לתפוס תקלות מוקדם, לפשט את תחזוקת הקוד, ולהבטיח ששינויים בקוד לא ישברו בטעות פונקציונליות קיימת.

## איך לעשות:

ב-PowerShell אין מסגרת בדיקות מובנית, אך Pester, מודול צד שלישי פופולרי, משמש רבות לכתיבה והרצת בדיקות. הנה איך להתחיל עם Pester לבדוק את הפונקציות שלך ב-PowerShell.

ראשית, התקן את Pester אם עדיין לא עשית זאת:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

לאחר מכן, נניח שיש לך פונקציה פשוטה של PowerShell שאתה רוצה לבדוק, שמורה כ-`MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

כדי לבדוק פונקציה זו עם Pester, צור סקריפט בדיקה בשם `MyFunction.Tests.ps1`. בסקריפט זה, השתמש בבלוקי `Describe` ו-`It` של Pester להגדיר את מקרי הבדיקה:

```powershell
# ייבוא הפונקציה לבדיקה
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tests" {
    It "מכפיל מספר ב-2 כאשר לא ניתן מכפיל" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "מכפיל נכון את המספר לפי המכפיל שניתן" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

כדי להריץ את הבדיקות, פתח את PowerShell, נווט אל התיקיה שבה מצוי הסקריפט שלך לבדיקה, והשתמש בפקודה `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

פלט לדוגמא יראה כך, מציין אם הבדיקות שלך עברו או נכשלו:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\נתיב\אל\MyFunction.Tests.ps1 204ms (182ms|16ms)
בדיקות הושלמו ב-204ms
בדיקות שעברו: 2, נכשלו: 0, דילגו: 0 לא רצו: 0
```

פלט זה מראה ששתי הבדיקות עברו, נותן לך ביטחון שהפונקציה `Get-MultipliedNumber` שלך מתנהגת כפי שצפוי בתרחישים שבדקת.
