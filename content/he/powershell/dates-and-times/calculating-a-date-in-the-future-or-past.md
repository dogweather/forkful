---
date: 2024-01-20 17:32:00.325665-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E0\u05D4\
  \ \u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05E7\u05D5\u05D3 \u05DC\u05D7\u05D9\
  \u05E9\u05D5\u05D1\u05D9 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1-PowerShell."
lastmod: '2024-03-13T22:44:39.720969-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05E7\u05D5\
  \u05D3 \u05DC\u05D7\u05D9\u05E9\u05D5\u05D1\u05D9 \u05EA\u05D0\u05E8\u05D9\u05DB\
  \u05D9\u05DD \u05D1-PowerShell."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

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
