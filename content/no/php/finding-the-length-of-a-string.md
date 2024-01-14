---
title:                "PHP: Å finne lengden på en streng."
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor 
Det å finne lengden på en streng kan være en viktig del av PHP-programmering, spesielt når man arbeider med tekstbehandling og manipulering av data. Det kan også være nyttig å vite lengden på en streng når man skal lage betingede uttrykk og løkker.

## Hvordan

For å finne lengden på en streng i PHP, kan du bruke funksjonen `strlen()`. Denne funksjonen tar inn en streng som parameter og returnerer lengden på strengen som et heltall (integer).

```PHP
<?php
$streng = "Hei, dette er en teststreng!";
echo strlen($streng); // Output: 28
?>
```

Du kan også bruke `echo` og `strlen()` sammen for å skrive ut lengden på en streng direkte i nettleseren.

```PHP
<?php
$navn = "Jonas";
echo "Navnet ditt, " . $navn . ", består av " . strlen($navn) . " bokstaver."; // Output: Navnet ditt, Jonas, består av 5 bokstaver.
?>
```

## Dypdykk

Det er viktig å huske at `strlen()` funksjonen tar hensyn til alle tegn i en streng, inkludert mellomrom og spesialtegn. Dette betyr at en streng som er 10 tegn lang kan ha en faktisk lengde på mer eller mindre enn 10, avhengig av hva den inneholder.

Hvis du trenger å finne lengden på en streng uten å inkludere mellomrom, kan du bruke `str_replace()` funksjonen for å fjerne disse først.

```PHP
<?php
$setning = "Jeg liker å programmere i PHP";
echo strlen($setning); // Output: 26

$setning = str_replace(" ", "", $setning); // Fjerner mellomrom
echo strlen($setning); // Output: 23
?>
```

Det er også viktig å merke seg at `strlen()` funksjonen kun fungerer for enkeltbyte-tegnsett. Hvis du arbeider med flerbyte-tegnsett som f.eks. kinesisk, kan du bruke `mb_strlen()` for å finne lengden på en streng.

## Se også

- [PHP Manual - strlen()](https://www.php.net/manual/en/function.strlen.php)
- [PHP Manual - str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP Manual - mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)