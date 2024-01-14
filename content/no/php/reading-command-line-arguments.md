---
title:                "PHP: Lesing av kommandolinjeargumenter"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne lese kommandolinje-argumenter er en svært nyttig ferdighet i PHP-programmering. Det lar deg lage mer dynamiske og interaktive skript, og kan spare tid og krefter ved å eliminere behovet for å stadig endre variabler i koden.

## Hvordan

For å lese kommandolinje-argumenter i PHP, kan du bruke funksjonen `getopt()`. Denne funksjonen tar to argumenter - en streng med argumentalternativer og en array som lagrer de returnerte verdiene.

```PHP
$options = getopt("a:b:c:");
```

I dette eksempelet er `a`, `b` og `c` argumentalternativer som kan brukes i kommandolinjen. Hvis du for eksempel kjører skriptet ditt ved å legge til `--a=1 --b=2` som argumenter, blir verdiene `1` og `2` lagret i `$options`-arrayen.

Det er også mulig å angi at et argument må ha en verdi ved å legge til kolon (`:`) etter argumentnavnet. For eksempel `getopt("f:")` vil kreve et argument som `--f=3` og lagre verdien `3` i `$options`-arrayen.

## Dypdykk

Ved å bruke `getopt()` i en løkke, kan du enkelt håndtere flere argumenter samtidig. Husk at hver gang `getopt()` kalles, vil den fjerne første argument i argumentlista. Derfor må du bruke en annen variabel for å lagre dine brukte argumenter for senere bruk.

En annen nyttig funksjon når du jobber med kommandolinje-argumenter er `count()` som lar deg telle antall elementer i en array. Dette kan være nyttig når du vil sjekke om et obligatorisk argument ble sendt inn.

For en fullstendig forståelse av hvordan `getopt()` fungerer og hvordan du kan bruke det i dine PHP-skript, sjekk ut PHPs dokumentasjon.

## Se også

- [PHPs dokumentasjon for `getopt()`](https://www.php.net/manual/en/function.getopt.php)
- [Tutorial: How to Read Command Line Arguments in PHP](https://www.tutorialrepublic.com/php-tutorial/php-command-line-arguments.php)
- [How to Use Command line Arguments in PHP](https://www.w3adda.com/php-tutorial/php-command-line-arguments)