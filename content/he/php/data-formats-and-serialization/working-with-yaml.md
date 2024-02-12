---
title:                "עבודה עם YAML"
aliases:
- /he/php/working-with-yaml.md
date:                  2024-02-03T19:26:37.244995-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, שמסמל "YAML Ain't Markup Language" (YAML אינו שפת סימון), הוא תבנית טקסטואלית קריאה לאדם לנירמול נתונים שמשמשת בעיקר עבור קבצי תצורה. מתכנתים בוחרים להשתמש ב-YAML בשל פשטותו וקריאותו, הופכים אותו לבחירה מעולה לאחסון הגדרות, פרמטרים ואף מבני נתונים מורכבים בצורה נוחה לניהול.

## איך לעשות:

PHP, כפי שהוא נמצא בגרסאותיו הנוכחיות, לא תומך בניתוח YAML כחלק מספרייתו הסטנדרטית. הדרך הפשוטה ביותר לעבוד עם YAML ב-PHP היא באמצעות רכיב YAML של Symfony או הרחבת PECL של `yaml`.

### שימוש ברכיב YAML של Symfony

ראשית, התקן את רכיב YAML של Symfony דרך Composer:

```bash
composer require symfony/yaml
```

לאחר מכן, תוכל לנתח וליצור תוכן ב-YAML כך:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// ניתוח YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// יצירת YAML ממערך
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

דוגמא לפלט בעת ניתוח:

```
Array
(
    [greet] => Hello, World!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

דוגמא לפלט בעת יצירה:

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### שימוש בהרחבת `yaml` PECL

אם אתה מעדיף, או אם דרישות הפרויקט שלך מאפשרות זאת, ההרחבה PECL יכולה להיות דרך יעילה נוספת לעבוד עם YAML. ראשית, ודא שההרחבה מותקנת:

```bash
pecl install yaml
```

לאחר מכן, הפעל אותה בתצורת ה-`php.ini` שלך:

```ini
extension=yaml.so
```

הנה איך לנתח ולפלוט YAML:

```php
<?php

// ניתוח YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// יצירת YAML ממערך
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

הפלט יהיה דומה לזה של רכיב Symfony, הממחיש את תפקידו של YAML כגשר בין פורמט קריא לאדם לבין מבני מערך PHP, מקל על תצורה ועיבוד נתונים קל יותר.
