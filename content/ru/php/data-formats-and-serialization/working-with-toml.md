---
title:                "Работа с TOML"
aliases:
- /ru/php/working-with-toml.md
date:                  2024-01-29T00:04:49.949619-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
TOML, что означает Tom's Obvious, Minimal Language (очевидный и минималистичный язык Тома), это формат данных, похожий на JSON или YAML, но более удобочитаемый для людей. Программисты используют его для конфигурационных файлов, потому что он простой и хорошо преобразуется в структуры данных.

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
