---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:49.949619-07:00
description: "TOML, \u0447\u0442\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442\
  \ Tom's Obvious, Minimal Language (\u043E\u0447\u0435\u0432\u0438\u0434\u043D\u044B\
  \u0439 \u0438 \u043C\u0438\u043D\u0438\u043C\u0430\u043B\u0438\u0441\u0442\u0438\
  \u0447\u043D\u044B\u0439 \u044F\u0437\u044B\u043A \u0422\u043E\u043C\u0430), \u044D\
  \u0442\u043E \u0444\u043E\u0440\u043C\u0430\u0442 \u0434\u0430\u043D\u043D\u044B\
  \u0445, \u043F\u043E\u0445\u043E\u0436\u0438\u0439 \u043D\u0430 JSON \u0438\u043B\
  \u0438 YAML, \u043D\u043E \u0431\u043E\u043B\u0435\u0435\u2026"
lastmod: '2024-03-13T22:44:45.253202-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u0447\u0442\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442\
  \ Tom's Obvious, Minimal Language (\u043E\u0447\u0435\u0432\u0438\u0434\u043D\u044B\
  \u0439 \u0438 \u043C\u0438\u043D\u0438\u043C\u0430\u043B\u0438\u0441\u0442\u0438\
  \u0447\u043D\u044B\u0439 \u044F\u0437\u044B\u043A \u0422\u043E\u043C\u0430), \u044D\
  \u0442\u043E \u0444\u043E\u0440\u043C\u0430\u0442 \u0434\u0430\u043D\u043D\u044B\
  \u0445, \u043F\u043E\u0445\u043E\u0436\u0438\u0439 \u043D\u0430 JSON \u0438\u043B\
  \u0438 YAML, \u043D\u043E \u0431\u043E\u043B\u0435\u0435 \u0443\u0434\u043E\u0431\
  \u043E\u0447\u0438\u0442\u0430\u0435\u043C\u044B\u0439 \u0434\u043B\u044F \u043B\
  \u044E\u0434\u0435\u0439."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

## Как это сделать:
Сначала убедитесь, что у вас установлена библиотека анализа TOML, например, `yosymfony/toml`. Давайте проанализируем файл TOML:

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

Пример вывода:

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

## Подробнее
TOML появился в 2013 году, его создал сооснователь GitHub Том Престон-Вернер как более дружелюбную к пользователю альтернативу XML и JSON для конфигурационных файлов. В то время как JSON прост для машин, структура TOML делает его удобным для восприятия человеком, без сложности YAML.

Альтернативы TOML включают JSON, YAML и XML. У каждого из них есть свои сильные стороны и сценарии применения. JSON повсеместно распространен и независим от языка; YAML более читаем и поддерживает комментарии, в то время как XML обширен и широко поддерживается.

При реализации TOML в PHP вы рассматриваете библиотеки, которые анализируют его содержимое в массивы или объекты PHP. `yosymfony/toml` — это PHP-парсер, который соответствует спецификации TOML v0.4.0. Чтобы не отставать, всегда проверяйте наличие новых парсеров или обновлений, которые поддерживают самую текущую версию TOML (v1.0.0 на момент моего последнего обновления).

## Смотрите также
- Спецификация TOML: <https://toml.io/>
- Парсер TOML для PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Сравнение форматов данных (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- Менеджер пакетов PHP (Composer): <https://getcomposer.org/>
