---
title:    "Javascript: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor

Det å kunne beregne en dato i fremtiden eller fortiden er et svært nyttig verktøy i programmering. Dette kan hjelpe til med å lage dynamiske kalendere, tidsstyring og mye mer.

## Hvordan

For å beregne en dato i fremtiden eller fortiden i JavaScript, kan du bruke Date-objektet. Dette objektet har flere metoder som kan hjelpe deg med dette, blant annet `setFullYear()` og `getDate()`.

For å beregne en dato i fremtiden, må vi først opprette et nytt Date-objekt og deretter bruke `setFullYear()`-metoden til å legge til eller trekke fra det ønskede antallet år. Vi kan deretter bruke `getDate()`-metoden til å hente ut dato-objektet og skrive ut resultatet.

```Javascript
// Opprett et nytt Date-objekt
let dato = new Date();

// Beregn dato 5 år frem i tid
dato.setFullYear(dato.getFullYear() + 5);

// Hent og skriv ut datoen
console.log(dato.getDate());
```

Dette vil skrive ut datoen som er 5 år fra nå i konsollen.

For å beregne en dato i fortiden, følger vi samme fremgangsmåte, men bruker denne gangen `setFullYear()`-metoden til å trekke fra ønsket antall år.

```Javascript
// Opprett et nytt Date-objekt
let dato = new Date();

// Beregn dato 5 år tilbake i tid
dato.setFullYear(dato.getFullYear() - 5);

// Hent og skriv ut datoen
console.log(dato.getDate());
```

Dette vil nå skrive ut datoen som var 5 år siden i konsollen.

## Dypdykk

For å forstå mer om hvordan dette fungerer, er det viktig å vite at JavaScript lagrer datoer som millisekunder siden 1. januar 1970 kl. 00:00:00 UTC. Dette er kjent som Epoch-tiden.

Når vi bruker `setFullYear()`-metoden, vil JavaScript beregne det nye året basert på Epoch-tiden og deretter returnere den nye datoen som et millisekund-stamp. Vi bruker derfor `getDate()`-metoden til å hente ut den faktiske datoen fra dette millisekund-stampet.

## Se også

- [Date-objektet på MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Hvordan konvertere datoer til millisekunder på Linux](https://linuxaria.com/howto/how-to-convert-a-date-to-epoch-time)
- [Datofunksjoner i JavaScript - en komplett guide](https://www.digitalocean.com/community/tutorials/js-javascript-date-time-functions)