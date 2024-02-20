---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:37.244995-07:00
description: "YAML, \u05E9\u05DE\u05E1\u05DE\u05DC \"YAML Ain't Markup Language\"\
  \ (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF\
  ), \u05D4\u05D5\u05D0 \u05EA\u05D1\u05E0\u05D9\u05EA \u05D8\u05E7\u05E1\u05D8\u05D5\
  \u05D0\u05DC\u05D9\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DC\u05D0\u05D3\u05DD\
  \ \u05DC\u05E0\u05D9\u05E8\u05DE\u05D5\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05E9\u05DE\u05E9\u05DE\u05E9\u05EA \u05D1\u05E2\u05D9\u05E7\u05E8 \u05E2\u05D1\
  \u05D5\u05E8 \u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05D1\u05D5\u05D7\u05E8\u05D9\u05DD\u2026"
lastmod: 2024-02-19 22:04:58.759683
model: gpt-4-0125-preview
summary: "YAML, \u05E9\u05DE\u05E1\u05DE\u05DC \"YAML Ain't Markup Language\" (YAML\
  \ \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF), \u05D4\
  \u05D5\u05D0 \u05EA\u05D1\u05E0\u05D9\u05EA \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\
  \u05DC\u05D9\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DC\u05D0\u05D3\u05DD \u05DC\
  \u05E0\u05D9\u05E8\u05DE\u05D5\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\
  \u05DE\u05E9\u05DE\u05E9\u05EA \u05D1\u05E2\u05D9\u05E7\u05E8 \u05E2\u05D1\u05D5\
  \u05E8 \u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05D1\u05D5\u05D7\u05E8\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
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
