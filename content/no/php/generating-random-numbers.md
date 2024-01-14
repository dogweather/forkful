---
title:                "PHP: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor vil noen generere tilfeldige tall? Det kan være nyttig for å lage sjansespill, testing av programmer og annet som krever tilfeldige verdier. I denne bloggposten skal vi se på hvordan vi kan generere tilfeldige tall i PHP.

## Hvordan

```PHP
<?php
// Genererer et tilfeldig heltall mellom 1 og 10
echo mt_rand(1, 10) . "\n";
// Eksempelutgang: 7
```

```PHP
<?php
// Genererer et tilfeldig desimaltall mellom 0 og 1
echo rand(0, 100) / 100 . "\n";
// Eksempelutgang: 0.57
```

Innenfor "```PHP ... ```" kodeblokker kan du se eksempler på hvordan du kan generere tilfeldige tall. Ved å bruke `mt_rand()` funksjonen kan du generere et tilfeldig heltall innenfor en gitt rekkevidde. Ved å bruke `rand()` funksjonen kan du generere et tilfeldig desimaltall. Disse funksjonene bruker en teknikk kalt "Mersenne twister" som sikrer at tallene er tilfeldige.

## Dypdykk

Nå som vi har sett på hvordan vi kan generere tilfeldige tall i PHP, kan vi dykke litt dypere og se på hva som skjer bak kulissene når vi kaller på disse funksjonene. "Mersenne twister" er en algoritme som bruker en spesifikk formel for å generere tall basert på en såkalt seed eller startverdi. Seed-verdien blir ofte basert på tiden når funksjonen blir kalt, slik at tallene blir mer tilfeldige. Det finnes også flere ulike metoder for å generere tilfeldige tall i PHP, som for eksempel `random_bytes()` og `openssl_random_pseudo_bytes()`.

## Se også

- [PHP Manual: Mersenne Twister](https://www.php.net/manual/en/function.mt-rand.php)
- [PHP Manual: random_bytes](https://www.php.net/manual/en/function.random-bytes.php)
- [PHP Manual: openssl_random_pseudo_bytes](https://www.php.net/manual/en/function.openssl-random-pseudo-bytes.php)