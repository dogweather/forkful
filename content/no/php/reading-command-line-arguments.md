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

## Hvorfor

Vi bruker ofte kommandolinjeargumenter når vi ønsker å gi et spesifikt sett med instruksjoner til et program uten å måtte endre koden. Ved å lese og tolke disse argumentene, kan vi gjøre programmene våre mer dynamiske og tilpasse dem til ulike behov.

## Hvordan

Kommandolinjeargumenter kan leses ved hjelp av en innebygd PHP-funksjon, `getopt()`, som returnerer en assosiativ array med argumentene og verdiene som ble gitt. Her er et eksempel på hvordan du kan lese og bruke argumenter i et PHP-skript:

```PHP
<?php
// På kommandolinjen: $ php script.php -t tips
$options = getopt("t:"); // Enkelt argument 't'
if (isset($options['t'])) { // Sjekker om argumentet finnes
    $tip = $options['t']; // Lagrer verdien i en variabel
    echo "Takk for tipset: $tip";
    // Output: Takk for tipset: tips
}
?>
```

Her bruker vi enkeltbokstaven `t` som et argument og gir det en verdi på kommandolinjen. Med `isset()`-funksjonen sjekker vi om argumentet eksisterer, og hvis det gjør det, lagrer vi verdien i en variabel og bruker den videre i koden. Output vil være: "Takk for tipset: tips".

## Deep Dive

Det er mange flerfunksjonelle måter å bruke `getopt()`-funksjonen på, som for eksempel å kunne godta flere argumenter samtidig og spesifisere om et argument krever en verdi eller ikke. Det er også mulig å legge til egne feilhåndteringsfunksjoner for å sikre at programmene våre håndterer ugyldige argumenter på en god måte.

Se dokumentasjonen for `getopt()`-funksjonen for mer informasjon om hvordan du kan lese og bruke kommandolinjeargumenter i PHP.

## Se også

- [Dokumentasjon for getopt()](https://www.php.net/manual/en/function.getopt.php)
- [Eksempler på PHP med kommandolinjeargumenter](https://www.php.net/manual/en/features.commandline.php)