---
title:                "Sjekker om en katalog eksisterer"
html_title:           "Elm: Sjekker om en katalog eksisterer"
simple_title:         "Sjekker om en katalog eksisterer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 
Å sjekke om en mappe eksisterer er en prosess der en kode spør operativsystemet om en spesifikk mappe finnes på en gitt plassering. Dette er nyttig for å unngå feil ved manipulering av filbaner, som å skrive til en mappe som ikke eksisterer.

## Hvordan:
Her er et eksempel på hvordan man kan sjekke om en mappe eksisterer i PHP:

```PHP
<?php
$mappe = '/sti/til/mappe';

if (file_exists($mappe) && is_dir($mappe)) {
   echo "Mappen eksisterer.";
} else {
   echo "Mappen eksisterer ikke.";
}
?>
```

Når du kjører koden, vil utskriften være enten "Mappen eksisterer." eller "Mappen eksisterer ikke.", basert på om mappen faktisk eksisterer.

## Deep Dive:
Historisk sett har metoden for å sjekke om en mappe eksisterer i PHP vært ganske rett frem. `file_exists()` funksjonen har eksistert siden PHP 4, og `is_dir()` siden PHP 3. 

Alternativt kan du sjekke om en mappe eksisterer med `glob()` funksjonen, men denne funksjonen kan være treigere når du jobber med mange filer. 

Ved implementeringen er det viktig å merke seg at `file_exists()` faktisk vil returnere `true` selv om det ikke er en mappe, men en fil. Derfor bruker vi `is_dir()` i tillegg for å sikre oss at stien peker til en mappe.

## Se Også:
1. Offisiell PHP Dokumentasjon for `file_exists()`: https://php.net/manual/en/function.file-exists.php
2. Offisiell PHP Dokumentasjon for `is_dir()`: https://php.net/manual/en/function.is-dir.php
3. "PHP Filesystem Functions": https://www.w3schools.com/php/php_ref_filesystem.asp
4. Diskusjon på Stackoverflow om sjekking av mappetilværelse: https://stackoverflow.com/questions/5428262/php-check-if-a-directory-exists