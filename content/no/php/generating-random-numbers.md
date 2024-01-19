---
title:                "Genererer tilfeldige tall"
html_title:           "PHP: Genererer tilfeldige tall"
simple_title:         "Genererer tilfeldige tall"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall er prosessen med å produsere tall sekvenser som ikke kan forutsigbart reproduseres. Som programmerere gjør vi dette for å sikre data sikkerhet, simuleringsautentisitet og for å drive statistiske analyser.

## Hvordan til:
Generering av tilfeldige tall er ganske enkel med PHP's innebygde `rand()` funksjonen. Her er et eksempel:
```PHP
<?php
echo rand()."\n";       // Viser et tilfeldig heltall mellom 0 og getrandmax()
echo rand(5, 15)."\n";  // Viser et tilfeldig heltall mellom 5 og 15
?>
```
Output for kvitta kunne være:

```
456238972
9
```

## Dybde Dykk:
Historisk sett har tilfeldige tall blitt generert ved hjelp av fysiske prosesser, slik som avstivede terninger eller myntkast. Med fremvekst av datamaskiner, har vi gått over til å bruke pseudorandom nummergeneratorer, som generatorfunksjonen `rand()`i PHP.

For den som søker en mer sikret alternativ, PHP tilbyr også `random_int()`eller `random_bytes()`, som genererer kryptografisk sikre tilfeldige tall eller bytes.

Dersom `getrandmax()` returnerer et tall som er for lavt for dine behov, bruk mt_rand(), som støtter større tall.

## Se Også:
For mer detaljer, sjekk ut PHP's egen dokumentasjon om random number generation:
[PHP: rand - Manual](https://www.php.net/manual/en/function.rand.php), 
[PHP: random_int - Manual](https://www.php.net/manual/en/function.random-int.php),
[PHP: random_bytes - Manual](https://www.php.net/manual/en/function.random-bytes.php).