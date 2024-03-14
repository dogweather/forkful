---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:48.023901-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05D3\u05D5\u05D1\u05E8 \u05E2\u05DC\
  \ \u05D6\u05D9\u05D4\u05D5\u05D9 \u05D5\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05DB\u05EA\u05D5\u05D1\u05D9\u05DD \u05D1\u05D8\u05E7\
  \u05E1\u05D8 \u05DC\u05E1\u05D5\u05D2 \u05E0\u05EA\u05D5\u05E0\u05D9 \u05EA\u05D0\
  \u05E8\u05D9\u05DA \u05E9-PowerShell \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05D1\
  \u05D9\u05DF \u05D5\u05DC\u05E2\u05D1\u05D5\u05D3 \u05D0\u05D9\u05EA\u05D5. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05EA\u05E4\u05E2\u05DC, \u05DC\u05E2\u05E6\u05D1\
  ,\u2026"
lastmod: '2024-03-13T22:44:39.714477-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05D3\u05D5\u05D1\u05E8 \u05E2\u05DC \u05D6\
  \u05D9\u05D4\u05D5\u05D9 \u05D5\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05DB\u05EA\u05D5\u05D1\u05D9\u05DD \u05D1\u05D8\u05E7\u05E1\
  \u05D8 \u05DC\u05E1\u05D5\u05D2 \u05E0\u05EA\u05D5\u05E0\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DA \u05E9-PowerShell \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05D1\u05D9\
  \u05DF \u05D5\u05DC\u05E2\u05D1\u05D5\u05D3 \u05D0\u05D9\u05EA\u05D5. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05EA\u05E4\u05E2\u05DC, \u05DC\u05E2\u05E6\u05D1,\u2026"
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
ניתוח תאריך ממחרוזת מדובר על זיהוי והמרת תאריכים כתובים בטקסט לסוג נתוני תאריך ש-PowerShell יכול להבין ולעבוד איתו. מתכנתים עושים זאת כדי לתפעל, לעצב, להשוות, או לחשב תאריכים, אשר הם משימות נפוצות בסקריפטים העוסקים בקבצי לוג, קלט משתמש, או עיבוד נתונים.

## איך לעשות:
PowerShell מקל על ניתוח תאריכים ממחרוזות באמצעות ה- `Get-Date` cmdlet ותאוצת הסוג `[datetime]`, אשר עובדים היטב עבור פורמטי תאריכים סטנדרטיים. עבור מחרוזות תאריך מורכבות או לא סטנדרטיות, ניתן להשתמש בשיטת `.ParseExact` של `[datetime]` כדי לציין את הפורמט המדויק.

### שימוש ב- `Get-Date` ו- `[datetime]`:
```powershell
# המרה פשוטה באמצעות Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**פלט דוגמה:**
```
Saturday, April 1, 2023 12:00:00 AM
```

```powershell
# שימוש בתאוצת הסוג [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**פלט דוגמה:**
```
Saturday, April 1, 2023 12:00:00 AM
```

### שימוש ב- `[datetime]::ParseExact` עבור פורמטים לא סטנדרטיים:
עבור פורמטים שאינם מזוהים באופן אוטומטי, ניתן להגדיר את הפורמט המדויק כדי להבטיח ניתוח נכון.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**פלט דוגמה:**
```
Saturday, April 1, 2023 2:00:00 PM
```

### שימוש בספריות של גורמים שלישיים
למרות ש-PowerShell בפני עצמו די חזק לניתוח תאריכים, עבור תרחישים מורכבים מאוד או פונקציונאליות נוספת, כדאי לחקור ספריות .NET כמו NodaTime, למרות שעבור רוב השימושים הטיפוסיים, היכולות הטבעיות של PowerShell יהיו מספיקות.

```powershell
# שימוש ב-NodaTime רק כדוגמה, שימו לב שצריך להוסיף את הספריה לפרויקט שלכם
# Install-Package NodaTime -Version 3.0.5
# שימוש ב-NodaTime לניתוח תאריך
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**הערת דוגמה:** הקוד הנ"ל הוא איור מושגי. בפועל, ודאו ש-NodaTime נוסף באופן תקין לפרויקט שלכם כדי שהסוגים והשיטות יהיו זמינים.
