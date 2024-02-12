---
title:                "עבודה עם YAML"
aliases:
- /he/ruby/working-with-yaml.md
date:                  2024-02-03T19:26:41.693240-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
YAML, שעומד ל-YAML Ain't Markup Language, משמש רבות ברובי לקבצי תצורה ולסידורי נתונים בזכות התצורה הקריאה לבני אדם. תכנתים נמשכים ל-YAML כאשר הם צריכים לאחסן או לשדר אובייקטים של נתונים בצורה קריאה אך מבנית, פשטות משימות כמו ניהול תצורה, אחסון נתונים, ושיתוף נתונים בין שפות.

## איך ל:
ברובי יש ספרייה מובנית בשם Psych לניתוח והפקת YAML. לשימוש בה, קודם כל צריך לדרוש את ספריית התקן של YAML. הנה דוגמה בסיסית להתחלה:

```ruby
require 'yaml'

# Hash לסידור
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# המרת ה-hash ל-YAML
yaml_data = person.to_yaml

puts yaml_data
```

**פלט דוגמה:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

לטעינת נתוני YAML בחזרה לאובייקט של רובי:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**פלט דוגמה:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### שימוש בספריות צד שלישי:

למרות שהספרייה הסטנדרטית מספיקה למשימות בסיסיות, לצרכים מורכבים ייתכן ותחפשו gem-ים של צד שלישי כמו 'safe_yaml'. לשימוש בספריות כאלו, קודם כל צריך להתקין את ה-gem:

```bash
gem install safe_yaml
```

אז, אתם יכולים להשתמש בה לטעינה בטוחה של נתוני YAML, בהפחתת סיכונים כמו יצירת אובייקט ממקורות שנשלטים על ידי המשתמש:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**פלט דוגמה:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

הגישה הזו משפרת את הביטחון של הטיפול ב-YAML שלכם, והופכת אותה לבחירה טובה ליישומים הטוענים נתוני YAML ממקורות לא מהימנים.
