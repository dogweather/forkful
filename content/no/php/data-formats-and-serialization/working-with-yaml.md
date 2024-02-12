---
title:                "Arbeider med YAML"
aliases:
- /no/php/working-with-yaml.md
date:                  2024-02-03T19:26:24.787610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeider med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML, som står for "YAML Ain't Markup Language", er et menneskelesbart data serialiseringsformat som vanligvis brukes for konfigurasjonsfiler. Programmerere velger å bruke YAML på grunn av dets enkelhet og lesbarhet, noe som gjør det til et utmerket valg for lagring av innstillinger, parametere, og til og med komplekse datastrukturer på en lett håndterbar måte.

## Hvordan:

PHP, i sine nåværende iterasjoner, støtter ikke parsing av YAML som en del av sitt standardbibliotek. Den mest strake veien til å arbeide med YAML i PHP er ved å bruke Symfony YAML-komponenten eller `yaml` PECL-utvidelsen.

### Bruke Symfony YAML-komponenten

Først, installer Symfony YAML-komponenten via Composer:

```bash
composer require symfony/yaml
```

Deretter kan du parse og dumpe YAML-innhold som følger:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// Parsing YAML
$yamlString = <<<YAML
greet: Hallo, Verden!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// Lage YAML fra et array
$array = [
    'greet' => 'Hallo, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

Eksempel på output når man parser:

```
Array
(
    [greet] => Hallo, Verden!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

Eksempel på output når man dumper:

```
greet: Hallo, YAML!
framework:
    name: Symfony
    language: PHP
```

### Bruke `yaml` PECL-utvidelsen

Om du foretrekker, eller om prosjektets krav tillater det, kan PECL-utvidelsen være en annen effektiv måte å arbeide med YAML på. Først, sørg for at utvidelsen er installert:

```bash
pecl install yaml
```

Deretter, aktiver den i din `php.ini`-konfigurasjon:

```ini
extension=yaml.so
```

Her er hvordan du parser og emitter YAML:

```php
<?php

// Parsing YAML
$yamlString = <<<YAML
greet: Hallo, Verden!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// Lage YAML fra et array
$array = [
    'greet' => 'Hallo, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

Outputen vil være lignende som med Symfony-komponenten, noe som illustrerer YAMLs rolle som en bro mellom menneskelesbart format og PHP array-strukturer, noe som lettere tilrettelegger for konfigurasjon og databehandling.
