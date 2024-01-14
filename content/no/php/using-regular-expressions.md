---
title:                "PHP: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang følt deg overveldet av alle de forskjellige tekstmanipuleringene du må gjøre i programmering? Da er det på tide å bli kjent med regulære uttrykk! Dette er et kraftig verktøy som kan gjøre det enkelt å finne og manipulere tekst, og det sparer deg for mye tid og frustrasjon.

## Slik bruker du regulære uttrykk i PHP

Å bruke regulære uttrykk i PHP er enkelt. Du trenger bare å bruke funksjonen `preg_match()` for å finne et mønster i en tekststreng. La oss si at du har en tekststreng som inneholder en e-postadresse, og du vil finne ut om den er gyldig eller ikke. Da kan du bruke følgende kode:

```PHP
$email = "example@email.com";

if (preg_match("/^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$/i", $email)) {
    echo "E-postadressen er gyldig.";
} else {
    echo "E-postadressen er ugyldig.";
}
```

I dette eksempelet bruker vi et regulært uttrykk for å validere e-postadressen ved hjelp av spesielle symboler og mønstre. Du kan også bruke regulære uttrykk til å erstatte deler av en tekststreng, finne og erstatte ord og utføre mange andre tekstmanipuleringer.

## Dypdykk i regulære uttrykk

For å forstå hvordan du kan bruke regulære uttrykk i PHP, er det viktig å lære litt om deres syntaks. Regulære uttrykk består av spesielle symboler og metakarakterer som representerer et visst mønster eller et sett med tegn. For eksempel kan `^` karakteren brukes for å indikere begynnelsen av en tekststreng, og `$` karakteren indikerer slutten. Ved å kombinere disse symbolene med andre tegn og tall, kan du lage avanserte mønstre for å matche eller erstatte tekst.

Det er også viktig å forstå at regulære uttrykk er case-sensitive, med mindre du spesifikt bruker flagg for å ignorere det. For eksempel er `A` og `a` forskjellige tegn i et regulært uttrykk, med mindre du bruker flagget `i` for å ignorere casing.

## Se også

- PHP manual på regex: https://www.php.net/manual/en/book.pcre.php
- Regex cheatsheet: https://www.rexegg.com/regex-quickstart.html
- Regex tester og validering: https://regex101.com/

For å lære mer om regulære uttrykk, kan du også lese på andre språk om dette kraftige verktøyet. Det er mye å utforske, men vi håper dette innlegget har gitt deg en god introduksjon til å bruke regulære uttrykk i PHP. Lykke til med å effektivisere tekstmanipulasjonen din!