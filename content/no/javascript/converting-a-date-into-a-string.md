---
title:                "Konvertere en dato til en streng"
html_title:           "Javascript: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å konvertere datoer til tekst er en viktig del av programmering, spesielt når man jobber med brukerinteraksjon og data lagring. Det er den eneste måten å vise datoer på en forståelig måte for brukeren og gjøre dem lesbare for datalagring og behandling. Det er derfor en essensiell ferdighet for alle Javascript-programmerere å kunne konvertere datoer til strenger.

## Slik gjør du det:
```javascript
// Eksempel på hvordan konvertere en dato til en streng
let date = new Date();
// Bruker built-in toLocaleString() metode
let stringDate = date.toLocaleString();
console.log(stringDate);
// Output: 07.01.2021, 14:38:50
```
```javascript
// Eksempel på hvordan formatere en dato i et spesifikt språk
let date = new Date();
// Bruker built-in toLocaleString() metode med spesifisering av språk og format
let stringDate = date.toLocaleString('no-NB', {dateStyle: 'full', timeStyle: 'short'});
console.log(stringDate);
// Output: torsdag 7. januar 2021 kl. 14:40
```

## Dypdykk:
Konvertering av datoer til tekst har vært en nødvendighet siden de tidligste programmeringsspråkene. I Javascript har vi flere innebygde metoder for å håndtere datoer, inkludert toLocaleString(), som gir deg muligheten til å formatere datoer basert på ulike språk og formater. Alternativt kan du bruke biblioteker som Moment.js for mer avansert og fleksibel håndtering av datoer.

Når det gjelder implementeringen av konvertering av datoer til strenger, er det viktig å være klar over at disse metodene gir deg muligheten til å formatere datoer, men de endrer ikke selve datatypen. Dette betyr at datoen fortsatt kan behandles som en dato i koden, men vil vises på en mer forståelig måte for brukerne.

## Se også:
- [Moment.js dokumentasjon](https://momentjs.com/docs/)
- [JS Date Objects](https://www.w3schools.com/js/js_dates.asp)