---
title:                "PHP: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med YAML kan være nyttig for utviklere som ønsker å organisere og lagre data på en strukturert måte. YAML er en enkel og leselig måte å representere data på, noe som gjør det enkelt å håndtere og behandle informasjon.

## Hvordan

For å jobbe med YAML i PHP, trenger du en parser som kan tolke YAML-data. Det finnes flere alternativer, men i dette eksemplet vil vi bruke Symfony Yaml-komponenten. Først må du installere komponenten med Composer:

```PHP
composer require symfony/yaml
```

Deretter kan du bruke følgende kode for å lese og skrive YAML-data:

```PHP
use Symfony\Component\Yaml\Yaml;

// Les YAML-data fra en fil
$data = Yaml::parseFile('/path/to/file.yaml');

// Skriv YAML-data til en fil
Yaml::dump($data, '/path/to/new_file.yaml');
```

Du kan også bruke Symfony Yaml-komponenten til å konvertere YAML-data til PHP-arrays og vice versa. For eksempel:

```PHP
use Symfony\Component\Yaml\Yaml;

// Konverter YAML til PHP-array
$array = Yaml::parse('navn: John Doe');

// Konverter PHP-array til YAML
$yaml = Yaml::dump(['navn' => 'John Doe']);
```

Du kan se et mer detaljert eksempel på hvordan du kan jobbe med YAML i PHP på [Symfony Yaml-komponentens offisielle dokumentasjon](https://symfony.com/doc/current/components/yaml.html).

## Dypere dykk

YAML støtter forskjellige datatyper som strenger, tall, arrayer og assosiative arrayer. Du kan også legge til kommentarer i YAML-filer som gjør det enklere å forstå og vedlikeholde koden.

Det er også verdt å merke seg at YAML har en hierarkisk struktur, noe som betyr at innrykk og pakking er viktig for å opprettholde datastruktur. Du kan lære mer om YAML-syntaks og beste praksis i [YAML-spesifikasjonen](https://yaml.org/spec/) og [Symfony Yaml-komponentens dokumentasjon](https://symfony.com/doc/current/components/yaml.html).

## Se også

* [YAML-spesifikasjonen](https://yaml.org/spec/)
* [Symfony Yaml-komponentens dokumentasjon](https://symfony.com/doc/current/components/yaml.html)
* [PHP.net - YAML](https://www.php.net/manual/en/book.yaml.php)