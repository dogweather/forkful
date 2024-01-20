---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "PowerShell: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# חישוב תאריך בעתיד או בעבר ב-PowerShell
מחבר: [שמך]

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פטיש של פרוגרמה שקופצת לתאריך מסוים מבלי או אחרי תאריך הנוכחי. מתכנתים עושים את זה בעיקר לניתוחי נתונים, תיזמון משימות או למנוע את פניית משתמש ללוח שנה!

## איך לעשות:

הנה דוגמאות קוד של פונקציות שמחשבות את התאריך המעודכן:

```PowerShell
# חישוב תאריך בעתיד
$FutureDate = (Get-Date).AddDays(15) # תוסיף 15 ימים לתאריך הנוכחי
$FutureDate

# חישוב תאריך בעבר
$PastDate = (Get-Date).AddDays(-15) # מחסיר 15 ימים מתאריך הנוכחי
$PastDate
```

ובהנחה שהיום הוא 2022-02-15, ההדפסה שלך תהיה בערך כך:

```PowerShell
2022-03-02 10:11:12
2022-01-31 10:11:12
```

## צלילה עמוקה

פעולת חישוב תאריך הוא מרכזי לרוב השפות התכנות, החל מ- COBOL  ועד JavaScript. PowerShell מספק הרבה אפשרויות נוספות, לדוגמא, חישוב שנים, חודשים, שניות, ועוד על ידי שימוש בפונקציות תאריך אחרות כמו `AddYears()`, `AddMonths()`, `AddSeconds()`, וכדומה.

אפשר גם להשתמש ב-PowerShell לחישוב זמן עם שעון בינלאומי מאוחד (UTC). רק צריך להשתמש בפקודה `(Get-Date).ToUniversalTime()` במקום `Get-Date`.

## ראה גם

- [DateTime Struct (System) | Microsoft Docs](https://docs.microsoft.com/he-il/dotnet/api/system.datetime?view=net-5.0)
- [Working with DateTime PowerShell](https://adamtheautomator.com/powershell-get-date/)