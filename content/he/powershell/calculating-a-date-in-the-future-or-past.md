---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:32:00.325665-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פשוט לומר למחשב, "תן לי תאריך כמה ימים לפני או אחרי תאריך מסוים." תכנותיים עושים זאת לתזמון אירועים, יעדי זמן וזיהוי חגים.

## איך לעשות:
הנה דוגמאות קוד לחישובי תאריכים ב-PowerShell:

```PowerShell
# הוספת ימים לתאריך נתון
$futureDate = (Get-Date).AddDays(10)
$futureDate

# הסרת ימים מתאריך נתון
$pastDate = (Get-Date).AddDays(-10)
$pastDate
```
דוגמאות לתוצאות:

```
יום שישי, 10 יוני 2023 18:30:00
יום שני, 30 מאי 2023 18:30:00
```

## צלילה עמוקה:
היכולת לחשב תאריכים היא חלק בסיסי בכל שפות תכנות. בעבר, תכנותיים היתה צורך לטפל ידנית בלוגיקה של שנים מעוברות וחודשים בעלי מספר ימים שונה. PowerShell הפך את המשימה לפשוטה עם הפונקציה `Get-Date` והמתודות של עצמי התאריך. חלופות כוללות ביבליות תאריך בשפות תכנות אחרות כמו Python, Java, או C#. ביצועים פנימיים של חישובי תאריכים מתבצעים בקפידה כדי להתחשב בפרטים כמו אזורי זמן ושינויי DST (Daylight Saving Time).

## ראה גם:
- [תיעוד ה-PowerShell על Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [DateTime Struct ב-.NET](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [תיעוד על אזורי זמן ב-.NET](https://docs.microsoft.com/en-us/dotnet/api/system.timezoneinfo?view=net-6.0)