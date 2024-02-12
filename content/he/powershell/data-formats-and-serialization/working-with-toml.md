---
title:                "עבודה עם TOML"
aliases: - /he/powershell/working-with-toml.md
date:                  2024-01-26T04:25:42.946521-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?

TOML, שמשמעותו "שפת מינימום וברורה של טום", היא פורמט סריאליזציה של נתונים שקל לקרוא בזכות הסמנטיקה הברורה שלו. מתכנתים משתמשים בו לקבצי קונפיגורציה, כיוון שהוא מצליח להציג איזון בין קריאות על ידי בני אדם לבין ידידותיות למכונה.

## איך לעשות:

ב-PowerShell, אין cmdlet טבעי לניתוח TOML. לרוב תשתמש במודול או תהפוך את TOML ל-JSON באמצעות כלי כמו `toml-to-json` אם אתה רוצה לעבוד עם PowerShell. הנה איך תעשה זאת באמצעות מודול פיקטיבי `PowerShellTOML`:

```PowerShell
# תחילה, התקן את המודול (דמיוני, להדגמה)
Install-Module PowerShellTOML

# ייבוא קובץ TOML
$config = Import-TomlConfig -Path './config.toml'

# גישה לערך
Write-Output $config.database.server

# תוכן TOML לדוגמא ב-'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# פלט לדוגמא:
# 192.168.1.1
```

## צלילה עמוקה

TOML נוצר על ידי טום פרסטון-ורנר, שותף מייסד של GitHub, כחלופה פשוטה יותר ל-XML ו-YAML עבור קבצי קונפיגורציה. הגרסה הראשונה שלו הופיעה ב-2013. TOML ניתן להשוואה ל-JSON אך מעוצב להיות ידידותי יותר לאנשים, מה שהופך אותו לבחירה טובה עבור קונפיגורציה שנתונה לתחזוקה על ידי בני אדם. בין החלופות נמנות YAML, JSON ו-XML.

מבחינת היישום, מודול PowerShell עבור TOML לרוב יהיה מעטפת סביב ספרייה של TOML שנכתבה בשפה בעלת אוריינטציה לביצועים גבוהים יותר כמו C#. ל-PowerShell אין תמיכה מובנית ל-TOML, ולכן מודול כזה נחוץ כדי להתממשק בנוחות עם פורמט ה-TOML.

## ראה גם

- תקן TOML: https://toml.io/en/
- מאגר GitHub עבור מודול `toml` של PowerShell (אם קיים במועד קריאת המאמר): https://github.com/powershell/PowerShellTOML
- הקדמה ל-TOML: https://github.com/toml-lang/toml
- השוואה של פורמטים לסריאליזציה של נתונים: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
