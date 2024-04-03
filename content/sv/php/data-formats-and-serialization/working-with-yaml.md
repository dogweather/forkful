---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:22.892013-07:00
description: "YAML, som st\xE5r f\xF6r \"YAML Ain't Markup Language\", \xE4r ett l\xE4\
  sbart data-serialiseringsformat som vanligtvis anv\xE4nds f\xF6r konfigurationsfiler.\
  \ Programmerare\u2026"
lastmod: '2024-03-13T22:44:38.016475-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r f\xF6r \"YAML Ain't Markup Language\", \xE4r ett l\xE4\
  sbart data-serialiseringsformat som vanligtvis anv\xE4nds f\xF6r konfigurationsfiler."
title: Att Arbeta med YAML
weight: 41
---

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
