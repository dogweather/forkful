---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:33.963596-07:00
description: "YAML, \u0449\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0454 \"YAML\
  \ Ain't Markup Language\" (YAML \u2013 \u0446\u0435 \u043D\u0435 \u043C\u043E\u0432\
  \u0430 \u0440\u043E\u0437\u043C\u0456\u0442\u043A\u0438), \u0454 \u043B\u044E\u0434\
  \u0438\u043D\u043E-\u0447\u0438\u0442\u0430\u0431\u0435\u043B\u044C\u043D\u0438\u043C\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0441\u0435\u0440\u0456\u0430\
  \u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\
  \u043A\u0438\u0439 \u0447\u0430\u0441\u0442\u043E \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F\u2026"
lastmod: 2024-02-19 22:05:08.499734
model: gpt-4-0125-preview
summary: "YAML, \u0449\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0454 \"YAML Ain't\
  \ Markup Language\" (YAML \u2013 \u0446\u0435 \u043D\u0435 \u043C\u043E\u0432\u0430\
  \ \u0440\u043E\u0437\u043C\u0456\u0442\u043A\u0438), \u0454 \u043B\u044E\u0434\u0438\
  \u043D\u043E-\u0447\u0438\u0442\u0430\u0431\u0435\u043B\u044C\u043D\u0438\u043C\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0441\u0435\u0440\u0456\u0430\
  \u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\
  \u043A\u0438\u0439 \u0447\u0430\u0441\u0442\u043E \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
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
