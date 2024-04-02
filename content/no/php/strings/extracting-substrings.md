---
date: 2024-01-20 17:46:26.574168-07:00
description: "Utdrag av substringer er n\xE5r du henter en del av en streng. Programmerere\
  \ gj\xF8r dette for \xE5 manipulere og analysere tekst, som \xE5 hente ut et brukernavn\
  \ fra\u2026"
lastmod: '2024-03-13T22:44:40.874646-06:00'
model: gpt-4-1106-preview
summary: "Utdrag av substringer er n\xE5r du henter en del av en streng. Programmerere\
  \ gj\xF8r dette for \xE5 manipulere og analysere tekst, som \xE5 hente ut et brukernavn\
  \ fra\u2026"
title: Uthenting av delstrenger
weight: 6
---

## Hva & Hvorfor?

Utdrag av substringer er når du henter en del av en streng. Programmerere gjør dette for å manipulere og analysere tekst, som å hente ut et brukernavn fra en e-postadresse.

## Hvordan:

La oss ta en titt. PHP har flere innebygde funksjoner for dette:

```PHP
$tekst = "Hei, dette er en eksempeltekst!";
// Bruk substr
$deltekst = substr($tekst, 0, 3); // "Hei"
echo $deltekst;

// Bruk mb_substr for flerbyte-tegnstøtte
$deltekst = mb_substr($tekst, 0, 3); // "Hei"
echo $deltekst;

// Hente ut brukernavn fra e-post
$epost = "ola.nordmann@example.com";
$brukernavn = strstr($epost, '@', true); // "ola.nordmann"
echo $brukernavn;
```

Eksempel utdata:

```
Hei
Hei
ola.nordmann
```

## Dypdykk

Før i tiden, når minne var dyrt, var substrings viktige for å spare plass. I PHP har `substr()` vært en støttespiller i lang tid. Med internasjonalisering ble `mb_substr()` viktig for støtte av multibyte-tegnsett, som UTF-8.

Alternativer til substrings inkluderer strengfunksjoner som `explode()`, `strtok()`, eller regulære uttrykk med `preg_match()` for mer komplekse mønstre.

`strstr()` kan også brukes for å finne en subtekst fra starten av en streng til første forekomst av et spesifisert tegn.

## Se Også

- PHP.net on `substr()`: https://www.php.net/manual/en/function.substr.php
- PHP.net on `mb_substr()`: https://www.php.net/manual/en/function.mb-substr.php
- PHP.net on `strstr()`: https://www.php.net/manual/en/function.strstr.php
- Interactive PHP Tutorial: https://www.learn-php.org/
- PHP The Right Way (for beste praksiser): https://phptherightway.com/
