---
title:                "עבודה עם YAML"
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט נתונים מבוסס טקסט שמתאפיין בקריאות לעין האנושית. תכניתנים עובדים עם YAML כדי להגדיר תצורה, יצירת תבניות ונתונים סטטיים באופן נוח ויעיל.

## איך לעשות:
```PowerShell
# טעינת נתוני YAML מקובץ
$yamlContent = Get-Content -Path 'config.yaml' -Raw
$yamlObject = ConvertFrom-Yaml $yamlContent

# עריכת אובייקט
$yamlObject.database.port = 3307

# ייצוא נתונים לקובץ YAML
$yamlObject | ConvertTo-Yaml | Set-Content -Path 'updated_config.yaml'
```

הפלט:
```yaml
# updated_config.yaml
database:
  host: localhost
  port: 3307
  name: my_database
```

## עיון נוסף
בעבר, תכניתנים ניזונו בעיקר מ-XML לתצורה. YAML, שהומצא בתחילת שנות ה-2000, הפך לאלטרנטיבה פופולרית בזכות פשטותו. היום, ישנן ספריות רבות ב-PowerShell לעבודה עם YAML, כמו `powershell-yaml` שמייצגת את המימוש הנפוץ ביותר. בחלופה ל-YAML ישנם פורמטים כמו JSON או TOML.

## ראו גם:
- [תיעוד ה-PowerShell-Yaml Module](https://github.com/cloudbase/powershell-yaml)
- [מבוא ל-YAML](https://learnxinyminutes.com/docs/yaml/)