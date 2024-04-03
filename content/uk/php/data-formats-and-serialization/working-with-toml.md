---
date: 2024-01-26 04:25:12.143486-07:00
description: "TOML, \u0449\u043E \u0454 \u0441\u043A\u043E\u0440\u043E\u0447\u0435\
  \u043D\u043D\u044F\u043C \u0432\u0456\u0434 Tom's Obvious, Minimal Language, \u0446\
  \u0435 \u0444\u043E\u0440\u043C\u0430\u0442 \u0434\u0430\u043D\u0438\u0445, \u043F\
  \u043E\u0434\u0456\u0431\u043D\u0438\u0439 \u0434\u043E JSON \u0430\u0431\u043E\
  \ YAML, \u0430\u043B\u0435 \u0431\u0456\u043B\u044C\u0448 \u0437\u0440\u0443\u0447\
  \u043D\u0438\u0439 \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F\
  \ \u043B\u044E\u0434\u044C\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.470476-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u0449\u043E \u0454 \u0441\u043A\u043E\u0440\u043E\u0447\u0435\u043D\
  \u043D\u044F\u043C \u0432\u0456\u0434 Tom's Obvious, Minimal Language, \u0446\u0435\
  \ \u0444\u043E\u0440\u043C\u0430\u0442 \u0434\u0430\u043D\u0438\u0445, \u043F\u043E\
  \u0434\u0456\u0431\u043D\u0438\u0439 \u0434\u043E JSON \u0430\u0431\u043E YAML,\
  \ \u0430\u043B\u0435 \u0431\u0456\u043B\u044C\u0448 \u0437\u0440\u0443\u0447\u043D\
  \u0438\u0439 \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u043B\
  \u044E\u0434\u044C\u043C\u0438."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

## Як це робити:
Перш за все, переконайтеся, що у вас встановлена бібліотека аналізатора TOML, наприклад, `yosymfony/toml`. Давайте розіб’ємо файл TOML:

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

Приклад виводу:

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

## Поглиблений аналіз
TOML з'явився у 2013 році, створений співзасновником GitHub Томом Престон-Вернером як більш дружній до користувача альтернатива XML і JSON для файлів конфігурації. Хоча JSON простий для машин, структура TOML робить його легким для людських очей без складності YAML.

Альтернативами TOML є JSON, YAML та XML. Кожен має свої сильні сторони та сценарії застосування. JSON є всюдисущим та незалежним від мови; YAML більш читабельний і підтримує коментарі, тоді як XML є обширним і широко підтримуваним.

При реалізації TOML у PHP ви шукаєте бібліотеки, які аналізують його вміст у масиви або об’єкти PHP. `yosymfony/toml` є PHP аналізатором, який відповідає специфікації v0.4.0 TOML. Щоб бути в курсі останніх подій, завжди перевіряйте наявність новіших аналізаторів або оновлень, які підтримують найсвіжішу версію TOML (v1.0.0 на момент мого останнього оновлення).

## Дивіться також
- Специфікація TOML: <https://toml.io/>
- Аналізатор TOML для PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Порівняння форматів даних (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- Менеджер пакетів PHP (Composer): <https://getcomposer.org/>
