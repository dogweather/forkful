---
title:    "PHP: Beregning av en dato i fremtiden eller fortiden"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å ville beregne en dato i fremtiden eller fortiden i PHP-programmering. Kanskje du ønsker å lage et dynamisk nettsted som viser ulike datoer basert på brukerens input, eller kanskje du ønsker å lage en applikasjon som sender påminnelser om kommende hendelser. Uansett årsak, er å kunne beregne datoer en viktig ferdighet for enhver PHP-utvikler.

## How To

Den enkleste måten å beregne en dato i PHP er ved å bruke funksjonen `date()`. Denne funksjonen tar to parametere: en formateringsstreng og en valgfri tidstempel-verdi. Formateringsstreng bestemmer hvordan datoen vil bli presentert, for eksempel 'd.m.Y' for å vise datoen som dag, måned og år. Tidstempel-verdien kan brukes til å beregne en dato basert på et spesifikt tidspunkt, for eksempel ved å legge til 10 dager til dagens dato.

```PHP
<?php
// Beregning av dato 10 dager frem i tid
$future_date = date('d.m.Y', strtotime('+10 days'));
echo $future_date; // Output: 07.07.2021

// Beregning av dato 5 år tilbake i tid
$past_date = date('d.m.Y', strtotime('-5 years'));
echo $past_date; // Output: 04.07.2016
?>
```

Det finnes også flere innebygde funksjoner i PHP for å beregne datoer, som for eksempel `strtotime()` og `strtotime()` som kan brukes til å konvertere en dato til et tidstempel og omvendt.

## Deep Dive

For å forstå mer om hvordan beregning av datoer fungerer i PHP, er det viktig å ha kjennskap til Unix-tid eller tidstempel. Dette er et tall som representerer antall sekunder som har gått siden 1. januar 1970 kl. 00:00:00 UTC. Dette brukes som en standard for å beregne datoer og tidsintervaller.

I tillegg til å bruke `date()` funksjonen, kan du også benytte deg av PHPs innebygde DateTime-klasse. Denne klassen har en rekke nyttige metoder for å beregne, formatere og manipulere datoer. Her er et eksempel på hvordan du kan beregne en dato ved å bruke DateTime-klasse og legge til et antall dager:

```PHP
<?php
$date = new DateTime();
$date->modify('+10 days');
echo $date->format('d.m.Y'); // Output: 07.07.2021
?>
```

Det finnes også mange tredjepartsbiblioteker for å hjelpe med beregning av datoer i PHP, som for eksempel Carbon eller Datetime.

## Se også

- [PHP.net: Date and time functions](https://www.php.net/manual/en/ref.datetime.php)
- [PHP.net: DateTime class](https://www.php.net/manual/en/class.datetime.php)
- [Carbon: A simple PHP API extension for DateTime](http://carbon.nesbot.com/)