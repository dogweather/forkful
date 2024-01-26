---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:56:36.195217-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Kommandolinjeargumenter lar deg kjøre PHP-skript med spesifikke parametere for ulike oppgaver. Å bruke disse er nødvendig for å lage fleksible skript som kan håndtere ulike inngangsdata dynamisk.

## How to:
PHP skript mottar kommandolinjeargumenter i `$argv` array og `$argc` variable. Her er et enkelt eksempel:

```php
<?php
// Sjekker om det er nok argumenter
if ($argc < 2) {
    echo "Bruk: php script.php [navn]\n";
    exit;
}

// Skriver ut det første argumentet
echo "Hei, " . $argv[1] . "!\n";

// Kjør: php script.php Verden
// Output: Hei, Verden!
```

## Deep Dive
Kommandolinjeargumenter har blitt brukt siden tidlige databehandlingsdager og er vanlige i mange programmeringsspråk. I PHP er `$argv` en array som inneholder argumentene og `$argc` er et tall som representerer antall argumenter. Bruk av disse er direkte og ubyråkratisk.

Alternativer for mer komplekse behov inkluderer `getopt()` funksjonen for flagg og verdier, eller tredjeparts biblioteker som Symfony's Console komponent.

Kjernedetaljer:
- `$argv[0]` er skriptnavnet.
- Argumentene starter fra `$argv[1]`.
- `getopt()` kan hente spesifikke options og verdier.

## See Also
- [PHP.net $argv](https://www.php.net/manual/en/reserved.variables.argv.php)
- [PHP.net $argc](https://www.php.net/manual/en/reserved.variables.argc.php)
- [PHP.net getopt](https://www.php.net/manual/en/function.getopt.php)
- [Symfony Console Component](https://symfony.com/doc/current/components/console.html)
