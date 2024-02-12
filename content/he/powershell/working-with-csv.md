---
title:                "עובדים עם CSV"
aliases:
- he/powershell/working-with-csv.md
date:                  2024-02-03T19:22:08.284289-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (ערכים מופרדים בפסיקים) היא משימה נפוצה לניהול וטיפול בנתונים בצורה מובנת וטבלאית. מתכנתים לעיתים קרובות מבצעים פעולה זו כדי לייבא, לייצא או לשנות נתונים ביעילות ליישומים שונים, כמו ניתוח נתונים, דיווח, או אפילו לתמוך ביישומי רשת.

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
