---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON ב-PowerShell משמשת לניתוח ויצירת מידע מבוסס-טקסט בפורמט הנפוץ JSON. תוכניתנים עושים זאת כיוון ש-JSON משמש סטנדרט המאפשר שיתוף נתונים בקלות בין לקוחות, שרתים ויישומים שונים.

## איך לעשות:
```PowerShell
# המרה מ-JSON לאובייקט PowerShell
$json = '{"שם": "דוד", "גיל": 30}'
$אובייקט = $json | ConvertFrom-Json
$אובייקט.שם

# תוצאה: דוד

# המרה מאובייקט PowerShell ל-JSON
$אובייקט = [PSCustomObject]@{
    שם = 'שרה'
    גיל = 32
}
$json = $אובייקט | ConvertTo-Json
Write-Output $json

# תוצאה:
# {
#     "שם":  "שרה",
#     "גיל":  32
# }

# שמירת JSON לקובץ
Set-Content -Path 'משתמש.json' -Value $json
```
## חפירה עמוקה
JSON (JavaScript Object Notation) היא פורמט התחלתי להחליף XML בשנת 2001, והפך לסטנדרט רשמי ב-2013. בניגוד ל-XML, JSON יותר קריא ופשוט. ב-PowerShell, הפקודות `ConvertFrom-Json` ו-`ConvertTo-Json` מיישמות את המרה ויצירת ה-JSON. ישנם חלופות בספריות שונות כמו Newtonsoft.Json ל.NET, אך ב-PowerShell הן לא נדרשות ברוב המקרים.

## ראו גם
- [מבוא ל-JSON](https://www.json.org/json-he.html)
- [תיעוד PowerShell על פקודת ConvertFrom-Json](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-json)
- [תיעוד PowerShell על פקודת ConvertTo-Json](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertto-json)
