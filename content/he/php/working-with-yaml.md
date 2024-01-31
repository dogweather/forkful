---
title:                "עבודה עם YAML"
date:                  2024-01-19
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה YAML ולמה זה משנה? YAML הוא פורמט קובץ המתאים לעבודה עם נתונים המיועד להיות קריא לאדם. תכניתנים משתמשים בו כיוון שהוא פשוט לעריכה ולקריאה, ומתאים לשילוב עם שפות תכנות רבות.

## How to:
לעבוד עם YAML ב-PHP? קל. השתמש בספריית `yaml` לקריאה וכתיבה.
התקן את ההרחבה אם עוד לא עשית זאת:
```bash
pecl install yaml
```

דוגמא לקריאת מידע מ-YAML:
```php
<?php
$yamlString = <<<YAML
עץ:
  תפוח: 3
  בננה: 5
  אפרסמון: 2
YAML;

$data = yaml_parse($yamlString);
print_r($data);
```

פלט:
```
Array
(
    [עץ] => Array
        (
            [תפוח] => 3
            [בננה] => 5
            [אפרסמון] => 2
        )
)
```

דוגמא ליצירת YAML ממערך:
```php
<?php
$array = [
  'דוד' => [
    'גובה' => 180,
    'משקל' => 80,
  ]
];

$yaml = yaml_emit($array);
echo $yaml;
```

פלט:
```
דוד:
  גובה: 180
  משקל: 80
```

## Deep Dive:
YAML (YAML Ain't Markup Language) הוא תכנון ידידותי לאדם לנתונים, שהתפרסם ב-2001. פורמטים חלופיים כוללים JSON ו-XML. חשוב לדעת ש-YAML תומך במבנים מורכבים כמו רשימות ומפות. ב-PHP, יישום יעיל דורש התקנת הרחבת PECL.

## See Also:
- מדריך על התקנת הרחבות PECL: https://www.php.net/manual/he/install.pecl.php
- מסמכי YAML רשמיים: https://yaml.org
- המדריך לספרייה `yaml` ב-PHP: https://www.php.net/manual/he/book.yaml.php

זהו, כל מה שצריך כדי לתפעל YAML ב-PHP.
