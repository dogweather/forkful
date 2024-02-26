---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:39.906993-07:00
description: "YAML, \u05D0\u05D5 YAML Ain't Markup Language, \u05D4\u05D9\u05D0 \u05E9\
  \u05E4\u05EA \u05D8\u05DB\u05E0\u05D5\u05DC\u05D5\u05D2\u05D9\u05D4 \u05DC\u05E1\
  \u05D9\u05D3\u05D5\u05E8\u05D9 \u05DE\u05D9\u05D3\u05E2 \u05E7\u05E8\u05D9\u05D0\
  \u05D9\u05DD \u05DC\u05D0\u05D3\u05DD. \u05EA\u05DB\u05E0\u05EA\u05E0\u05D9\u05DD\
  \ \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DC\u05E7\u05D1\u05E6\u05D9\
  \ \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05D5\u05D4\u05E2\u05D1\u05E8\u05EA \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\u05DF \u05E9\u05E4\u05D5\u05EA.\u2026"
lastmod: '2024-02-25T18:49:37.971366-07:00'
model: gpt-4-0125-preview
summary: "YAML, \u05D0\u05D5 YAML Ain't Markup Language, \u05D4\u05D9\u05D0 \u05E9\
  \u05E4\u05EA \u05D8\u05DB\u05E0\u05D5\u05DC\u05D5\u05D2\u05D9\u05D4 \u05DC\u05E1\
  \u05D9\u05D3\u05D5\u05E8\u05D9 \u05DE\u05D9\u05D3\u05E2 \u05E7\u05E8\u05D9\u05D0\
  \u05D9\u05DD \u05DC\u05D0\u05D3\u05DD. \u05EA\u05DB\u05E0\u05EA\u05E0\u05D9\u05DD\
  \ \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DC\u05E7\u05D1\u05E6\u05D9\
  \ \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05D5\u05D4\u05E2\u05D1\u05E8\u05EA \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\u05DF \u05E9\u05E4\u05D5\u05EA.\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
---

{{< edit_this_page >}}

## מה ולמה?
YAML, או YAML Ain't Markup Language, היא שפת טכנולוגיה לסידורי מידע קריאים לאדם. תכנתנים לעיתים קרובות משתמשים בה לקבצי הגדרות והעברת נתונים בין שפות. הפשטות והנגישות שלה הופכות אותה לפופולרית במיוחד למשימות שקשורות להגדרת סביבות, אפליקציות או שירותים בהם התצורות הן קריטיות וצריכות להיות קלות להבנה ולעריכה.

## איך לעשות:
PowerShell, כברירת מחדל, אינה מגיעה עם cmdlet מובנה לפענוח YAML, אך היא עובדת בחלקות עם YAML כאשר אתה מנצל את המודול `powershell-yaml` או ממיר YAML לאובייקט של PowerShell באמצעות `ConvertFrom-Json` בשילוב עם כלי כמו `yq`.

### שימוש במודול `powershell-yaml`:
ראשית, התקן את המודול:
```PowerShell
Install-Module -Name powershell-yaml
```

לקרוא קובץ YAML:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

לכתוב אובייקט של PowerShell לקובץ YAML:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

דוגמה ל`output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### פענוח YAML עם `yq` ו`ConvertFrom-Json`:
דרך נוספת כוללת את השימוש ב`yq`, מעבד שורת פקודה קל ונייד עבור YAML. `yq` יכול להמיר YAML ל-JSON, אותו PowerShell יכול לנתח באופן טבעי.

ראשית, וודא ש`yq` מותקן במערכת שלך.
אז הרץ:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

שיטה זו שימושית במיוחד למשתמשים העובדים בסביבות רב-מערכתיות או עדיפים להשתמש ב-JSON בפנים PowerShell.
