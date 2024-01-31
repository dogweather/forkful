---
title:                "עבודה עם YAML"
date:                  2024-01-19
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט קל לקריאה לעיבוד נתונים, תומך במבנים כמו מפות, רשימות וסקלרים. תוכניתנים משתמשים ב-YAML בגלל קריאות ופשטות הפורמט, ואפשרות להטמעה קלה בתוכניות Ruby.

## איך לעשות:
```Ruby
require 'yaml'

# יצירת מבנה נתונים ברובי (Ruby Data Structure)
data = {
  "name" => "Yossi",
  "role" => "Developer",
  "skills" => ["Ruby", "Rails", "Docker"]
}

# שמירת הנתונים לקובץ YAML
File.open("employee.yaml", "w") { |file| file.write(data.to_yaml) }

# קריאת נתונים מקובץ YAML
yaml_content = YAML.load(File.read("employee.yaml"))
puts yaml_content
```
פלט (Output):
```
{"name"=>"Yossi", "role"=>"Developer", "skills"=>["Ruby", "Rails", "Docker"]}
```

## טבילה עמוקה
YAML (YAML Ain't Markup Language) נוצר ב-2001 כתחליף קל ל-XML. הפשטות של YAML מאפשרת עריכה ושיתוף נוח של קובצי הגדרות וקונפיגורציות. חלופות ל-YAML כוללות JSON ו-TOML. YAML משתלב טוב עם Ruby באמצעות הספרייה 'yaml', המבוססת על libyaml, ספרייה נפוצה לניתוח YAML.

## ראו גם:
- YAML רשמי: [https://yaml.org](https://yaml.org)
- מסמך המתאר את הספרייה 'yaml' ב-Ruby: [https://ruby-doc.org/stdlib-3.1.0/libdoc/yaml/rdoc/YAML.html](https://ruby-doc.org/stdlib-3.1.0/libdoc/yaml/rdoc/YAML.html)
- ג'יטהאב של libyaml: [https://github.com/yaml/libyaml](https://github.com/yaml/libyaml)
