---
title:                "Работа с YAML"
aliases:
- ru/php/working-with-yaml.md
date:                  2024-01-29T00:05:01.542316-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

YAML - это удобный для человека формат сериализации данных. Программисты используют его для файлов конфигурации, обмена данными и хранения из-за его читаемости и простоты.

## Как:

Чтобы работать с YAML в PHP, вам понадобится расширение `yaml`. Вот быстрый старт:

**Установить расширение YAML** (если оно не установлено):
```bash
pecl install yaml
```

**Загрузка расширения**:
Убедитесь, что ваш `php.ini` включает:
```ini
extension=yaml
```

**Разбор YAML**: 
```php
<?php
$yamlString = "
settings:
  database: MySQL
  host: localhost";

$array = yaml_parse($yamlString);

print_r($array);
```
**Пример выходных данных**:
```
Array
(
    [settings] => Array
        (
            [database] => MySQL
            [host] => localhost
        )
)
```

**Создание YAML**:
```php
<?php
$array = [
  'settings' => [
    'database' => 'MySQL',
    'host' => 'localhost'
  ]
];

$yamlString = yaml_emit($array);
echo $yamlString;
```
**Пример выходных данных**:
```
settings:
  database: MySQL
  host: localhost
```

## Глубокое погружение:

YAML, что расшифровывается как "YAML - это не язык разметки", фокусируется на данных и структурах данных и превосходит там, где языки вроде XML могут быть слишком сложными. Впервые он был выпущен в 2001 году. Альтернативами являются JSON и XML; YAML часто предпочитают за его читабельность для человека. Расширение `yaml` для PHP использует библиотеку `libyaml`, обеспечивая быстрый разбор и создание.

## Смотрите также:

- Официальная документация PHP по расширению YAML: https://www.php.net/manual/ru/book.yaml.php
- Официальный сайт YAML: https://yaml.org
- Сравнение форматов сериализации данных: https://ru.wikipedia.org/wiki/Сравнение_форматов_сериализации_данных
