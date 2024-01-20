---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא יצירת תסריטים שמוודאים שהקוד שלך עובד כמצופה. תכנותים בדיקות כדי לאתר באגים מוקדם, לשפר איכות הקוד ולהבטיח ששינויים לא ישברו פונקציונליות קיימת.

## איך לעשות:
הנה דוגמא לכתיבת בדיקה פשוטה ב-PowerShell עם Pester, הפריימוורק לבדיקות:

```PowerShell
# התקנת Pester
Install-Module -Name Pester -Force -SkipPublisherCheck

# קובץ פונקציות שלך: MyFunctions.ps1
function Get-MagicNumber {
    return 42
}

# קובץ בדיקות: MyFunctions.Tests.ps1
Describe "Get-MagicNumber Tests" {
    It "returns the correct magic number" {
        .\MyFunctions.ps1
        Get-MagicNumber | Should -Be 42
    }
}

# הפעלת הבדיקות
Invoke-Pester .\MyFunctions.Tests.ps1
```

פלט הדוגמה:

```
Starting discovery in 1 files.
Discovery finished in 248ms.
Running tests...
[+] C:\Path\To\Tests\MyFunctions.Tests.ps1 42ms (36ms|5ms)
Tests completed in 42ms
Tests Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## עיון מעמיק:
Pester הוא הפריימוורק המוביל לבדיקות ב-PowerShell וזמין מתוך החבילה מאז גרסה 4.בעבר, תכנותי היו משתמשים בבדיקות ידניות או שימוש בסקריפטים פשוטים ללא פריימוורק מסוים. חלופה פופולרית לPester היא NUnit, אשר מותאם יותר לפרויקטים ב.NET, אך לא כל כך פשוט לשילוב בסביבת PowerShell. פרטי היישום של Pester כוללים features כמו Mocking וCode Coverage, שעוזרים לבנות בדיקות מדויקות וחזקות יותר.

## ראה גם:
- [הדוקומנטציה הרשמית של Pester](https://pester.dev/)
- [קורס הדרכה ב-YouTube לPester](https://www.youtube.com/playlist?list=PLfeA8kIs7CocdF8M8OZ_NXa_M7GJ8KeSs)