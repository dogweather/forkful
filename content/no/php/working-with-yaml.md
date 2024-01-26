---
title:                "Arbeid med YAML"
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, "YAML Ain't Markup Language", er et dataformat som ligner på JSON, men er enklere å lese for mennesker. Programmerere bruker det fordi det er optimalt for konfigurasjonsfiler og datautveksling mellom språk.

## How to:
Med PHP kan du lett jobbe med YAML filer gjennom Symfony-komponenten `Yaml`. Her er hvordan du leser og skriver YAML.

```PHP
<?php
require_once 'vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// Lese inn en YAML-fil
$yaml = Yaml::parse(file_get_contents('eksempel.yaml'));

// Skrive ut array
print_r($yaml);

// Konvertere et array til en YAML-streng
$array = ['name' => 'Ola', 'age' => 34, 'city' => 'Oslo'];
$yaml = Yaml::dump($array);

// Skriv YAML-strengen til fil
file_put_contents('ny_eksempel.yaml', $yaml);
```

Output etter å lese vil være et PHP-array. Etter å skrive, en YAML-formattert tekstfil.

## Deep Dive
YAML kom først i 2001 og ville forenkle datautveksling og lesbarhet. Alternativer inkluderer XML og JSON. Symfony YAML bruker `libyaml` under hetta hvis tilgjengelig, for fart. Pass på tab vs space-feil; YAML bruker spaces.

## See Also
- Symfony YAML-komponent dokumentasjon: [https://symfony.com/doc/current/components/yaml.html](https://symfony.com/doc/current/components/yaml.html)
- YAML offisielle nettside: [https://yaml.org/](https://yaml.org/)
- PHP.net sin YAML-funksjoner: [https://www.php.net/manual/en/book.yaml.php](https://www.php.net/manual/en/book.yaml.php)
