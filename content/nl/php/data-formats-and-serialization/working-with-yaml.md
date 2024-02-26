---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:07.694343-07:00
description: "YAML is een door mensen leesbaar gegevensserialisatieformaat. Programmeurs\
  \ gebruiken het voor configuratiebestanden, gegevensuitwisseling en opslag\u2026"
lastmod: '2024-02-25T18:49:48.254747-07:00'
model: gpt-4-0125-preview
summary: "YAML is een door mensen leesbaar gegevensserialisatieformaat. Programmeurs\
  \ gebruiken het voor configuratiebestanden, gegevensuitwisseling en opslag\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML is een door mensen leesbaar gegevensserialisatieformaat. Programmeurs gebruiken het voor configuratiebestanden, gegevensuitwisseling en opslag vanwege de leesbaarheid en eenvoud.

## Hoe te:

Om met YAML in PHP te werken, heb je de `yaml`-extensie nodig. Hier is een snelle opstart:

**Installeer YAML-extensie** (indien niet geïnstalleerd):
```bash
pecl install yaml
```

**Laad Extensie**:
Zorg ervoor dat je `php.ini` het volgende bevat:
```ini
extension=yaml
```

**YAML Parsen**: 
```php
<?php
$yamlString = "
instellingen:
  database: MySQL
  host: localhost";

$array = yaml_parse($yamlString);

print_r($array);
```
**Voorbeelduitvoer**:
```
Array
(
    [instellingen] => Array
        (
            [database] => MySQL
            [host] => localhost
        )
)
```

**YAML Maken**:
```php
<?php
$array = [
  'instellingen' => [
    'database' => 'MySQL',
    'host' => 'localhost'
  ]
];

$yamlString = yaml_emit($array);
echo $yamlString;
```
**Voorbeelduitvoer**:
```
instellingen:
  database: MySQL
  host: localhost
```

## Diepere Duik:

YAML, wat staat voor "YAML Ain't Markup Language", richt zich op gegevens en gegevensstructuren, en het blinkt uit waar talen als XML misschien te complex zouden zijn. Het werd voor het eerst uitgebracht in 2001. Alternatieven omvatten JSON en XML; YAML wordt vaak verkozen vanwege de leesbaarheid voor mensen. De `yaml`-extensie van PHP haakt in op de `libyaml` bibliotheek, wat zorgt voor snelle parsing en emitting.

## Zie Ook:

- PHP Officiële YAML Extensie Documentatie: https://www.php.net/manual/en/book.yaml.php
- YAML Officiële Site: https://yaml.org
- Vergelijking van gegevensserialisatieformaten: https://nl.wikipedia.org/wiki/Vergelijking_van_gegevensserialisatieformaten
