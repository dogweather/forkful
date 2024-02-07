---
title:                "Praca z YAML"
date:                  2024-02-03T19:26:16.578358-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

YAML, co oznacza "YAML Ain't Markup Language", to format serializacji danych czytelny dla człowieka, który jest powszechnie używany do plików konfiguracyjnych. Programiści decydują się na wykorzystanie YAML ze względu na jego prostotę i czytelność, co czyni go doskonałym wyborem do przechowywania ustawień, parametrów, a nawet złożonych struktur danych w łatwo zarządzalnej formie.

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
