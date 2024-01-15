---
title:                "Å laste ned en nettside"
html_title:           "PHP: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor?

Å laste ned en nettside er en viktig del av utviklingen og testingen av nettsider. Det lar utviklere se hvordan nettsiden fungerer i forskjellige nettlesere og på forskjellige enheter. Det kan også være nyttig for å lagre en kopi av en nettside for senere bruk eller referanse.

## Hvordan?

Det er flere måter å laste ned en nettside ved hjelp av PHP. Her er et eksempel på hvordan du kan laste ned nettsiden for nettsideutvikleren A List Apart:

```PHP
<?php

// URL for nettsiden du ønsker å laste ned
$url = "https://alistapart.com/";

// Opprett et nytt cURL-objekt
$ch = curl_init();

// Sett URLen som skal lastes ned
curl_setopt($ch, CURLOPT_URL, $url);

// Lagre resultatet i en variabel istedenfor å skrive det ut på skjermen
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

// Utfør cURL-forespørselen
$resultat = curl_exec($ch);

// Sjekk om det var noen feil
if(curl_errno($ch)){
    echo 'Feil: ' . curl_error($ch);
}

// Lukk cURL-objektet
curl_close($ch);

// Skriv ut resultatet
echo $resultat;
```

Dette eksempelet bruker cURL, som er et vanlig verktøy for å sende og motta data via ulike protokoller som HTTP og HTTPS. Ved å bruke cURL til å laste ned nettsiden, vil resultatet bli lagret i variabelen "$resultat", og deretter skrevet ut på skjermen.

## Dypdykk

En alternativ måte å laste ned en nettside ved hjelp av PHP er å bruke funksjonen "file_get_contents()". Denne funksjonen lar deg hente innholdet på en nettside og lagre det i en variabel. Her er et eksempel på hvordan du kan bruke dette til å laste ned nettsiden for TutsPlus:

```PHP
<?php

// URL for nettsiden du ønsker å laste ned
$url = "https://tutsplus.com/";

// Bruk file_get_contents() til å laste ned innholdet på nettsiden
$resultat = file_get_contents($url);

// Skriv ut resultatet
echo $resultat;
```

Det er viktig å merke seg at begge disse metodene bare laster ned innholdet på nettsiden, ikke hele nettsiden. Det vil si at eventuelle ressurser som bilder og CSS-filer ikke vil bli lastet ned. For å få en fullstendig kopi av nettsiden, kan du i stedet bruke et verktøy som HTTrack eller WebCopy.

## Se også

- [cURL documentation](https://www.php.net/manual/en/book.curl.php)
- [file_get_contents() documentation](https://www.php.net/manual/en/function.file-get-contents.php)
- [HTTrack](https://www.httrack.com/)
- [WebCopy](https://www.cyotek.com/cyotek-webcopy)