---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.284289-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05E7\u05D5\u05D1\u05E5 CSV, \u05D9\u05E9\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1-cmdlet `Import-Csv`. Cmdlet \u05D6\
  \u05D4 \u05E7\u05D5\u05E8\u05D0 \u05D0\u05EA \u05D4\u05E7\u05D5\u05D1\u05E5 \u05D5\
  \u05DE\u05DE\u05D9\u05E8 \u05D0\u05D5\u05EA\u05D5 \u05DC\u05D0\u05D5\u05D1\u05D9\
  \u05D9\u05E7\u05D8\u05D9\u05DD \u05DE\u05D5\u05EA\u05D0\u05DE\u05D9\u05DD \u05D0\
  \u05D9\u05E9\u05D9\u05EA \u05E9\u05DC PowerShell \u05E2\u05D1\u05D5\u05E8 \u05DB\
  \u05DC \u05E9\u05D5\u05E8\u05D4."
lastmod: '2024-03-13T22:44:39.735177-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05E7\u05D5\u05D1\
  \u05E5 CSV, \u05D9\u05E9 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1-cmdlet `Import-Csv`."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

## איך לעשות:


### קריאה מקובץ CSV
כדי לקרוא מקובץ CSV, יש להשתמש ב-cmdlet `Import-Csv`. Cmdlet זה קורא את הקובץ וממיר אותו לאובייקטים מותאמים אישית של PowerShell עבור כל שורה.

```powershell
# ייבוא קובץ CSV
$data = Import-Csv -Path "C:\Data\users.csv"
# הצגת התוכן
$data
```

**דוגמא לפלט:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### כתיבה לקובץ CSV
לחלופין, כדי לכתוב נתונים לתוך קובץ CSV, משתמשים ב-cmdlet `Export-Csv`. Cmdlet זה לוקח אובייקטים קלטים וממיר אותם לפורמט CSV.

```powershell
# יצירת אובייקט לייצוא
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# ייצוא לקובץ CSV
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

לאחר הביצוע, נוצר קובץ בשם `new_users.csv` עם הנתונים שסופקו.

### סינון ושינוי תוכן CSV
כדי לסנן או לשנות את הנתונים מקובץ CSV, יש להשתמש ביכולות הניהול של אובייקטים ב-PowerShell. לדוגמה, כדי לבחור רק משתמשים מעל גיל מסוים ומעיר מסוימת:

```powershell
# ייבוא וסינון נתונים
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# הצגת הנתונים המסוננים
$filteredData
```

**דוגמא לפלט:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### שימוש בספריות צד שלישי
אף על פי ש-cmdlets המובנים של PowerShell לרוב מספיקים למשימות נפוצות, פעולות מורכבות יותר עשויות להרוויח מספריות או כלים של צד שלישי. עם זאת, למטרות טיפול סטנדרטי בקובצי CSV, כמו קריאה, כתיבה, סינון, או מיון, cmdlets המובנים של PowerShell כמו `Import-Csv` ו-`Export-Csv` לרוב מציעים תפקוד חזק בלי צורך בספריות נוספות.
