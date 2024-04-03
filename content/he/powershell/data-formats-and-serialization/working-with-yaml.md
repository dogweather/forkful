---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:39.906993-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: PowerShell, \u05DB\
  \u05D1\u05E8\u05D9\u05E8\u05EA \u05DE\u05D7\u05D3\u05DC, \u05D0\u05D9\u05E0\u05D4\
  \ \u05DE\u05D2\u05D9\u05E2\u05D4 \u05E2\u05DD cmdlet \u05DE\u05D5\u05D1\u05E0\u05D4\
  \ \u05DC\u05E4\u05E2\u05E0\u05D5\u05D7 YAML, \u05D0\u05DA \u05D4\u05D9\u05D0 \u05E2\
  \u05D5\u05D1\u05D3\u05EA \u05D1\u05D7\u05DC\u05E7\u05D5\u05EA \u05E2\u05DD YAML\
  \ \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05DE\u05E0\u05E6\u05DC \u05D0\u05EA\
  \ \u05D4\u05DE\u05D5\u05D3\u05D5\u05DC `powershell-yaml` \u05D0\u05D5 \u05DE\u05DE\
  \u05D9\u05E8\u2026"
lastmod: '2024-03-13T22:44:39.731930-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, \u05DB\u05D1\u05E8\u05D9\u05E8\u05EA \u05DE\u05D7\u05D3\u05DC\
  , \u05D0\u05D9\u05E0\u05D4 \u05DE\u05D2\u05D9\u05E2\u05D4 \u05E2\u05DD cmdlet \u05DE\
  \u05D5\u05D1\u05E0\u05D4 \u05DC\u05E4\u05E2\u05E0\u05D5\u05D7 YAML, \u05D0\u05DA\
  \ \u05D4\u05D9\u05D0 \u05E2\u05D5\u05D1\u05D3\u05EA \u05D1\u05D7\u05DC\u05E7\u05D5\
  \u05EA \u05E2\u05DD YAML \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05DE\u05E0\
  \u05E6\u05DC \u05D0\u05EA \u05D4\u05DE\u05D5\u05D3\u05D5\u05DC `powershell-yaml`\
  \ \u05D0\u05D5 \u05DE\u05DE\u05D9\u05E8 YAML \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\
  \u05E7\u05D8 \u05E9\u05DC PowerShell \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ `ConvertFrom-Json` \u05D1\u05E9\u05D9\u05DC\u05D5\u05D1 \u05E2\u05DD \u05DB\u05DC\
  \u05D9 \u05DB\u05DE\u05D5 `yq`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

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
