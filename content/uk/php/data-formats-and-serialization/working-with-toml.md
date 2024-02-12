---
title:                "Робота з TOML"
aliases:
- /uk/php/working-with-toml.md
date:                  2024-01-26T04:25:12.143486-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і Чому?
TOML, що є скороченням від Tom's Obvious, Minimal Language, це формат даних, подібний до JSON або YAML, але більш зручний для читання людьми. Програмісти використовують його для файлів конфігурації, тому що він простий і добре перетворюється на структури даних.

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
