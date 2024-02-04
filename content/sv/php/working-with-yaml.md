---
title:                "Att Arbeta med YAML"
date:                  2024-02-03T19:26:22.892013-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, som står för "YAML Ain't Markup Language", är ett läsbart data-serialiseringsformat som vanligtvis används för konfigurationsfiler. Programmerare väljer att använda YAML på grund av dess enkelhet och läsbarhet, vilket gör det till ett utmärkt val för att lagra inställningar, parametrar och till och med komplexa datastrukturer i en lättförvaltad form.

## Hur man gör:

PHP, i sina nuvarande iterationer, stöder inte tolkning av YAML som en del av sitt standardbibliotek. Det enklaste sättet att arbeta med YAML i PHP är genom att använda Symfony YAML-komponenten eller `yaml` PECL-tillägget.

### Använda Symfony YAML-komponenten

Först, installera Symfony YAML-komponenten via Composer:

```bash
composer require symfony/yaml
```

Sedan kan du tolka och dumpa YAML-innehåll på följande sätt:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// Tolka YAML
$yamlString = <<<YAML
greet: Hej, världen!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// Skapa YAML från en array
$array = [
    'greet' => 'Hej, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

Exempelutskrift vid tolkning:

```
Array
(
    [greet] => Hej, världen!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

Exempelutskrift vid dumpning:

```
greet: Hej, YAML!
framework:
    name: Symfony
    language: PHP
```

### Använda `yaml` PECL-tillägget

Om du föredrar, eller om ditt projekt tillåter, kan PECL-tillägget vara ett annat effektivt sätt att arbeta med YAML. Först, se till att tillägget är installerat:

```bash
pecl install yaml
```

Aktivera sedan det i din `php.ini`-konfiguration:

```ini
extension=yaml.so
```

Så här tolkar och emitterar du YAML:

```php
<?php

// Tolka YAML
$yamlString = <<<YAML
greet: Hej, världen!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// Skapa YAML från en array
$array = [
    'greet' => 'Hej, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

Utskriften kommer att likna Symfony-komponentens, vilket illustrerar YAML:s roll som en bro mellan människoläsbar form och PHP-arraystrukturer, underlättar enklare konfiguration och datahantering.
