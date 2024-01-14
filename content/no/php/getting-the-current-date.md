---
title:    "PHP: Hente gjeldende dato"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen er en viktig del av mange programmeringsprosjekter. Enten det er for å vise riktig dato til brukeren, eller for å utføre beregninger basert på dato, er det viktig å kunne hente den nåværende datoen i en kode.

## Hvordan

For å få den nåværende datoen i PHP kan du bruke funksjonen `date()`. Denne funksjonen tar to argumenter: format og valgfri timestamp.

```PHP
$date = date("d-m-Y"); // lagrer den nåværende datoen i variabelen $date
echo $date; // output: 22-06-2021
```

Den første parameteren, format, bestemmer hvordan datoen skal vises. Her brukes "d-m-Y" for å vise dag, måned og år i rekkefølgen. Du kan bruke ulike bokstaver for å vise forskjellige deler av datoen, som for eksempel "D" for å vise dagen som en forkortelse (f.eks. "Tir" for tirsdag) eller "F" for å vise måneden som et helt ord ("Juni").

Den andre parameteren, timestamp, er en valgfri verdi som angir hvilken dato som skal hentes. Standardverdien er dagens dato, men du kan også bruke en timestamp for å få en spesifikk dato (mer om dette i "Deep Dive"-delen).

## Deep Dive

PHP har en innebygd funksjon som heter `time()`, som returnerer antall sekunder siden 1. januar 1970. Dette kalles en Unix-timestamp og brukes ofte til å sammenligne datoer eller beregne tidsintervaller.

Her er et eksempel på hvordan du kan bruke `time()` sammen med `date()` for å få den nåværende datoen, men en måned tidligere:

```PHP
$now = date("d-m-Y"); // lagrer nåværende dato i variabelen $now
$one_month_ago = date("d-m-Y", time() - (30 * 24 * 60 * 60)); // trekker fra 30 dager i antall sekunder
echo $one_month_ago; // output: 22-05-2021
```

Som du ser, er det enkelt å manipulere datoen ved å bruke timestamps og riktig format. Dette kan være nyttig for å generere rapporter basert på tilpassede tidsintervaller eller for å utføre spesielle oppgaver som krever bearbeiding av datoer.

## Se også

- [PHP-dokumentasjon: date()](https://www.php.net/manual/en/function.date.php)
- [PHP-dokumentasjon: time()](https://www.php.net/manual/en/function.time.php)
- [Komplett oversikt over bokstaver for datofunksjoner i PHP](https://www.php.net/manual/en/function.date.php#example-2611)