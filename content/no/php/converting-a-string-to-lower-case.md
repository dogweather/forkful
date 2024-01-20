---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Å konvertere en streng til små bokstaver er en prosess for å endre alle tegn i en streng til deres tilsvarende lowercase former. Programmere gjør dette for å normalisere data, bedre data sammenligning og for å optimalisere søketid.

## Hvordan:

Her er hvordan du konvertere en streng til små bokstaver i PHP:

```PHP
$tema = "PROGRAMMERING I PHP";
$tema_sma = strtolower($tema);
echo $tema_sma;
```
Utskriften av dette blir:

```PHP
programmering i php
```
Dette er fordi funksjonen strtolower i PHP konverterer alle alfabetiske tegn til små bokstaver.

## Dypdykk

Før innføringen av PHP 4.0, kunne PHP bare håndtere strenger på ASCII-nivå. Men med utgivelsen av PHP 4.0, kom støtte for multibyte-streng funksjoner som lar oss håndtere strenger utenfor ASCII-området. Stkrlower() faller under denne kategorien.

Du kan også bruke `mb_strtolower()` for multibyte-sikker case folding. Dette er nyttig når du arbeider med multibyte tegnsett som UTF-8.

Innholdet for begge disse funksjonene kan være i form av variabler, datatyper og hardkodede strenger.

## Se også

- [PHP: strtolower - Manual](https://www.php.net/manual/en/function.strtolower.php)
- [PHP: mb_strtolower - Manual](https://www.php.net/manual/en/function.mb-strtolower.php)