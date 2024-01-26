---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם CSV פירושה ליצור, לקרוא, ולערוך קבצים עם נתונים המפורדים בפסיקים. תוכניתנים עושים את זה כדי להתמודד עם נתונים בפורמט פשוט ונפוץ, דבר שמקל על הייבוא והיצוא ממערכות שונות.

## איך לעשות:
קריאת CSV:
```PowerShell
$csvData = Import-Csv -Path "data.csv"
$csvData
```
יצירת CSV:
```PowerShell
$person = @{'Name'='דוד'; 'Age'=30; 'City'='תל אביב'}
$newCsvRow = New-Object PSObject -Property $person
$newCsvRow | Export-Csv "data.csv" -NoTypeInformation -Append
```
עריכת CSV:
```PowerShell
$csvData = Import-Csv -Path "data.csv"
$csvData[0].Age = 31
$csvData | Export-Csv "data.csv" -NoTypeInformation
```

## עיון מעמיק
CSV, ראשי תיבות של Comma-Separated Values, הוא פורמט שהחל להימצא בשנות ה-70 ונועד לאחסון נתונים טבלאיים. חלופות פופולריות כוללות XML ו-JSON, שמאפשרים ייצוג מורכב יותר של מבני נתונים. עבודה עם CSV ב-PowerShell נעשית באמצעות פקודות כמו `Import-Csv` ו-`Export-Csv`, שמאפשרות טעינה ושמירה של נתוני CSV בקלות.

## ראו גם
- [תיעוד רשמי של PowerShell ל-`Import-Csv`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- [תיעוד רשמי של PowerShell ל-`Export-Csv`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv)
