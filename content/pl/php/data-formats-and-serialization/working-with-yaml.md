---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:16.578358-07:00
description: "Jak to zrobi\u0107: PHP, w swoich obecnych iteracjach, nie obs\u0142\
  uguje parsowania YAML jako cz\u0119\u015B\u0107 swojej standardowej biblioteki.\
  \ Najprostszym sposobem pracy z\u2026"
lastmod: '2024-03-13T22:44:35.517868-06:00'
model: gpt-4-0125-preview
summary: "PHP, w swoich obecnych iteracjach, nie obs\u0142uguje parsowania YAML jako\
  \ cz\u0119\u015B\u0107 swojej standardowej biblioteki."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
PHP, w swoich obecnych iteracjach, nie obsługuje parsowania YAML jako część swojej standardowej biblioteki. Najprostszym sposobem pracy z YAML w PHP jest użycie komponentu YAML Symfony lub rozszerzenia `yaml` PECL.

### Użycie komponentu Symfony YAML
Najpierw zainstaluj komponent YAML Symfony za pomocą Composera:

```bash
composer require symfony/yaml
```

Następnie możesz parsować i zrzucać zawartość YAML w następujący sposób:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// Parsowanie YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// Tworzenie YAML z tablicy
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

Przykładowe wyjście przy parsowaniu:

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

Przykładowe wyjście przy zrzucaniu:

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### Użycie rozszerzenia `yaml` PECL
Jeśli wolisz, lub jeśli wymagania twojego projektu na to pozwalają, rozszerzenie PECL może być kolejnym efektywnym sposobem na pracę z YAML. Najpierw upewnij się, że rozszerzenie jest zainstalowane:

```bash
pecl install yaml
```

Następnie włącz je w swojej konfiguracji `php.ini`:

```ini
extension=yaml.so
```

Oto jak parsować i emitować YAML:

```php
<?php

// Parsowanie YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// Tworzenie YAML z tablicy
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

Wyjście będzie podobne do komponentu Symfony, ilustrując rolę YAML jako mostu między formatem czytelnym dla człowieka a strukturami tablic PHP, co ułatwia łatwiejszą konfigurację i obsługę danych.
