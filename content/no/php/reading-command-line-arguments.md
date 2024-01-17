---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "PHP: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av kommandolinjeargumenter er en vanlig praksis blant PHP-programmerere for å gjøre det mulig å utføre handlinger basert på informasjon som er gitt via kommandolinjen. Dette gjør det mulig å lage fleksible og tilpasningsdyktige skript ved å endre argumentene som blir gitt til programmet.

## Hvordan:
For å lese kommandolinjeargumenter i PHP, kan du bruke funksjonen `getopt()` sammen med en liste over ønskede argumenter. Et eksempel på bruk av denne funksjonen kan være som følger:

```PHP
<?php
$options = getopt("f:d:h", [
  'file:',
  'directory:',
  'help'
]);
print_r($options);
```

Når dette skriptet blir kjørt med argumentet `-f test.txt -d /home -h`, vil følgende output bli generert:

```
Array
(
    [f] => test.txt
    [d] => /home
    [h] => 1
    [file] => test.txt
    [directory] => /home
    [help] => 1
)
```

Som du kan se, gir funksjonen `getopt()` en assosiativ array av argumenter og deres verdier. Dette gjør det enkelt å håndtere flere argumenter og deres verdier.

## Dypdykk:
Lesing av kommandolinjeargumenter har vært en vanlig praksis i programmering verden i lang tid. Det finnes også alternative måter å håndtere argumenter på, som for eksempel bruk av funksjonen `$_SERVER['argv']`. Dette gir en array av argumenter gitt via kommandolinjen, men det er opp til programmereren å håndtere disse argumentene på riktig måte.

En viktig ting å huske på når du leser kommandolinjeargumenter er å validere og behandle dem forsvarlig. Dette er spesielt viktig når du arbeider med sensitive data eller utfører handlinger som kan påvirke systemet ditt. Det kan også være lurt å dokumentere hvilke argumenter skriptet ditt forventer å motta, slik at andre brukere kan bruke det riktig.

## Se også:
- [PHP: getopt()](https://www.php.net/manual/en/function.getopt.php)
- [PHP: $_SERVER](https://www.php.net/manual/en/reserved.variables.server.php)