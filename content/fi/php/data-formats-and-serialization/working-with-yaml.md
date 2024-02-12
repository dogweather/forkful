---
title:                "Työskentely YAML:n kanssa"
aliases:
- /fi/php/working-with-yaml.md
date:                  2024-02-03T19:26:45.746025-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

YAML, jonka nimi tulee sanoista "YAML Ain't Markup Language", on ihmisen luettavissa oleva datan serialisointiformaatti, jota käytetään yleisesti konfiguraatiotiedostoissa. Ohjelmoijat suosivat YAML:n käyttöä sen yksinkertaisuuden ja luettavuuden vuoksi, mikä tekee siitä erinomaisen valinnan asetusten, parametrien ja jopa monimutkaisten datarakenteiden tallentamiseen helposti hallittavassa muodossa.

## Kuinka:

PHP ei nykyisissä versioissaan tue YAML:n jäsentämistä osana sen vakio kirjastoa. Helpoin tapa työskennellä YAML:n kanssa PHP:ssä on käyttämällä Symfony YAML -komponenttia tai `yaml` PECL-laajennusta.

### Käyttäen Symfony YAML -komponenttia

Asenna ensin Symfony YAML -komponentti Composerin kautta:

```bash
composer require symfony/yaml
```

Sitten voit jäsentää ja luoda YAML-sisältöä seuraavasti:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// YAML:n jäsentäminen
$yamlString = <<<YAML
greet: Hei, maailma!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// YAML:n luominen taulukosta
$array = [
    'greet' => 'Hei, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

Esimerkkituloste jäsentäessä:

```
Array
(
    [greet] => Hei, maailma!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

Esimerkkituloste luodessa:

```
greet: Hei, YAML!
framework:
    name: Symfony
    language: PHP
```

### Käyttäen `yaml` PECL-laajennusta

Jos haluat, tai jos projektin vaatimukset sallivat, PECL-laajennus voi olla toinen tehokas tapa työskennellä YAML:n kanssa. Varmista ensin, että laajennus on asennettu:

```bash
pecl install yaml
```

Ota sitten käyttöön `php.ini`-konfiguraatiossasi:

```ini
extension=yaml.so
```

Näin jäsentät ja muodostat YAML:ää:

```php
<?php

// YAML:n jäsentäminen
$yamlString = <<<YAML
greet: Hei, maailma!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// YAML:n luominen taulukosta
$array = [
    'greet' => 'Hei, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

Tulos on samankaltainen kuin Symfony-komponentin kanssa, mikä havainnollistaa YAML:n roolia sillassa ihmisen luettavan muodon ja PHP-taulukkorakenteiden välillä, helpottaen konfiguraation ja datan käsittelyä.
