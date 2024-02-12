---
title:                "Робота з YAML"
aliases:
- /uk/php/working-with-yaml/
date:                  2024-02-03T19:26:33.963596-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

YAML, що означає "YAML Ain't Markup Language" (YAML – це не мова розмітки), є людино-читабельним форматом серіалізації даних, який часто використовується для файлів конфігурації. Програмісти обирають YAML через його простоту і читабельність, що робить його відмінним варіантом для збереження налаштувань, параметрів, а також навіть складних структур даних в легко керованій формі.

## Як:

У своїх поточних версіях PHP не підтримує аналіз YAML як частину своєї стандартної бібліотеки. Найбільш простий спосіб працювати з YAML у PHP - використання компонента Symfony YAML або розширення `yaml` PECL.

### Використання компонента Symfony YAML

Спершу встановіть компонент Symfony YAML через Composer:

```bash
composer require symfony/yaml
```

Потім ви можете аналізувати та виводити вміст YAML наступним чином:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// Аналіз YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// Створення YAML з масиву
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

Зразок виводу при аналізі:

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

Зразок виводу при створенні:

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### Використання розширення `yaml` PECL

Якщо ви віддаєте перевагу, або якщо вимоги вашого проєкту дозволяють, розширення PECL може бути ще одним ефективним способом роботи з YAML. Спершу переконайтеся, що розширення встановлено:

```bash
pecl install yaml
```

Потім увімкніть його у вашій конфігурації `php.ini`:

```ini
extension=yaml.so
```

Ось як аналізувати і генерувати YAML:

```php
<?php

// Аналіз YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// Створення YAML з масиву
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

Вивід буде схожий до компоненту Symfony, ілюструючи роль YAML як мосту між людино-читабельним форматом і структурами масиву PHP, спрощуючи конфігурацію та обробку даних.
