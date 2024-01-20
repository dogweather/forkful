---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

הייצור של מספרים אקראיים הוא תהליך בו נוצרים מספרים באופן שאינו ניתן לחיזוי. מתכנתים משתמשים בכך לתסריטים שונים, כמו יצירת מידע מדגם, בחינת קוד, משחקים או אסימולציות.

## איך לעשות:

על ידי משתמש בפונקציה Get-Random, אפשר ליצור מספר אקראי.

```PowerShell
# יצירת מספר אקראי בין 1 ל-100
מספר_אקראי = Get-Random -Minimum 1 -Maximum 100
Write-Output $מספר_אקראי
```

בהרצה תוצאה כזאת או דומה לה תתקבל:

```PowerShell
58
```

## צלילה עמוקה:

מאז האינטרנט הקדומה, הייצור של מספרים אקראיים נפוץ במחשבים. ישן, וכעת לא משמש, הגישה ל-COM ותוסף ה-Scriptlet, אך הגישה הנוכחית של PowerShell הרבה יותר יעילה.

שים לב, Get-Random מניב מספר אקראי בין ה- Minimum ו- Maximum, לא כולל ה- Maximum. זהו היבט של המנגנון .NET שעליו מתבסס PowerShell.

## ראה גם:

1. [Microsoft Docs: Get-Random](https://docs.microsoft.com/he-il/powershell/module/microsoft.powershell.utility/get-random)
2. [Stack Overflow: Generating Random Numbers in PowerShell](https://stackoverflow.com/questions/4753702/generating-random-numbers-in-powershell)