---
title:                "עבודה עם YAML"
aliases: - /he/powershell/working-with-yaml.md
date:                  2024-02-03T19:26:39.906993-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
