---
title:                "פרסום תאריך ממחרוזת"
aliases:
- /he/powershell/parsing-a-date-from-a-string/
date:                  2024-02-03T19:15:48.023901-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
