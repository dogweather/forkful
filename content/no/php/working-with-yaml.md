---
title:                "Å jobbe med yaml"
html_title:           "PHP: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med YAML i PHP? Vel, YAML (YAML Ain't Markup Language) er et enkelt, leselig og portabelt tekstformat som brukes til å lagre og overføre data. Det er spesielt nyttig for å konfigurere programvare og for å håndtere strukturerte data.

## Hvordan

For å begynne å arbeide med YAML i PHP, må du sørge for å installere den siste versjonen av PHP på datamaskinen din. Deretter kan du følge disse trinnene:

1. Begynn med å laste ned og installere et YAML-bibliotek for PHP, som for eksempel "Symfony YAML".
2. Opprett et nytt PHP-dokument og importer YAML-biblioteket ved å bruke "require" -funksjonen.
3. Lag et assosiativt array med dataene du ønsker å lagre i YAML-filen din.
4. Bruk YAML-biblioteket til å konvertere arrayet til en YAML-streng.
5. Lagre YAML-strengen i en fil ved hjelp av "file_put_contents" -funksjonen.

Et eksempel på hvordan koden kan se ut:

```PHP
<?php
require 'vendor/autoload.php';
use Symfony\Component\Yaml\Yaml;

$data = [
    'navn' => 'Andrea',
    'alder' => 25,
    'hobbyer' => ['lesing', 'fotografering', 'matlaging']
];

$yamlString = Yaml::dump($data);

file_put_contents('minfil.yaml', $yamlString);
```

Når du kjører dette skriptet, vil det lage en YAML-fil med navnet "minfil.yaml", som vil inneholde følgende:

```YAML
navn: Andrea
alder: 25
hobbyer:
    - lesing
    - fotografiering
    - matlaging
```

Dette er et enkelt eksempel, men det viser hvordan du kan bruke YAML til å organisere og lagre data. Du kan også bruke YAML til å konfigurere programmer ved å definere ulike verdier for ulike miljøer, for eksempel utvikling, testing og produksjon.

## Dypdykk

YAML støtter også avanserte funksjoner som inkludering av filer og referanser til andre verdier. Du kan også bruke mye nyttig syntaks når du jobber med komplekse datastrukturer. For en dypere forståelse av alle mulighetene som finnes i YAML, kan du sjekke ut dokumentasjonen for YAML-spesifikasjonen.

## Se også

* Symfony YAML dokumentasjon: https://symfony.com/doc/current/components/yaml.html
* Offisiell YAML-spesifikasjon: https://yaml.org/spec/
* Composer - Dependency Manager for PHP: https://getcomposer.org/