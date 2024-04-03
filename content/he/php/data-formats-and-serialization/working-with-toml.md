---
date: 2024-01-26 04:25:00.823972-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05E8\u05D0\u05E9\u05D9\u05EA, \u05D5\u05D5\
  \u05D3\u05D0\u05D5 \u05E9\u05D4\u05EA\u05E7\u05E0\u05EA\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 TOML, \u05DB\u05DE\u05D5 `yosymfony/toml`.\
  \ \u05D1\u05D5\u05D0\u05D5 \u05E0\u05E0\u05EA\u05D7 \u05E7\u05D5\u05D1\u05E5 TOML."
lastmod: '2024-03-13T22:44:39.519587-06:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05D0\u05E9\u05D9\u05EA, \u05D5\u05D5\u05D3\u05D0\u05D5 \u05E9\u05D4\
  \u05EA\u05E7\u05E0\u05EA\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05E0\u05D9\
  \u05EA\u05D5\u05D7 TOML, \u05DB\u05DE\u05D5 `yosymfony/toml`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

## איך ל:
ראשית, וודאו שהתקנתם ספריית ניתוח TOML, כמו `yosymfony/toml`. בואו ננתח קובץ TOML:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

דוגמה לפלט:

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```

## עיון מעמיק
TOML נוצר ב-2013, יצירתו של שותף המנכ"ל של GitHub, טום פרסטון-ורנר, כחלופה ידידותית יותר למשתמש ביחס ל-XML ול-JSON לקבצי תצורה. בעוד ש-JSON פשוט למכונות, מבנה ה-TOML קל לעיני האדם, ללא המורכבות של YAML.

חלופות ל-TOML כוללות JSON, YAML, ו-XML. לכל אחת מהם יתרונות ותרחישי שימוש ספציפיים. JSON נפוץ בכל מקום ואינו תלוי שפה; YAML קריאה יותר ותומכת בהערות, בעוד ש-XML מקיפה ונתמכת ברחבי.

כאשר מיישמים TOML ב-PHP, אתם מחפשים ספריות שמנתחות את תוכנו למערכים או אובייקטים של PHP. `yosymfony/toml` היא ספריית ניתוח PHP שמתאימה לגרסה 0.4.0 של המפרט TOML. כדי להמשיך ולעדכן, בדקו תמיד ניתוחים חדשים או עדכונים שתומכים בגרסה העדכנית ביותר של TOML (גרסה 1.0.0 נכון לעדכון האחרון שלי).

## ראו גם
- מפרט TOML: <https://toml.io/>
- ניתוח TOML ל-PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- השוואת פורמטים נתונים (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- מנהל החבילות של PHP (Composer): <https://getcomposer.org/>
