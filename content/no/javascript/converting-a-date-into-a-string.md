---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en dato til en streng innebærer å endre datatypen fra Date til String. Programmerere gjør dette for å forenkle datohåndtering og for å fremstille datoer på en lesbar og forståelig måte for brukere.

## Hvordan:

Her er et utvalg JavaScript-kodesekvenser som illustrerer prosessen:

```javascript
let date = new Date();
let stringDate = date.toString();
console.log(stringDate);
```

Dette vil utløse en utskrift som ligner dette:

```javascript
'Fri Oct 15 2021 12:00:00 GMT+0200 (Central European Summer Time)'
```

## Dypdykk

Historisk sett har konvertering av dato til streng vært en del av programmeringsspråkene lenge. JavaScript, introdusert i 1995, inkluderte funksjonen i dens Date objekt.

Det finnes flere metoder for å konvertere en dato til en streng i JavaScript, inkludert toLocaleString(), toISOString() og toDateString(). Hver metode presenterer datoen på forskjellige formater og kan være mer passende avhengig av den spesifikke bruken.

For eksempel, vil toLocaleString() konvertere datoen til en streng som er formatert på språket i den nåværende locale - meget nyttig for internasjonale applikasjoner.

```javascript
let date = new Date();
let stringDate = date.toLocaleString('no-NO');
console.log(stringDate);
```

Eksempelutskrift:

```javascript
'15.10.2021, 12:00:00' 
```

Dato til streng konvertering skjer i bakgrunnen ved bruk av JavaScripts innebygde method ToString(), som returnerer en streng som representerer det spesifiserte Date objektet.

## Se Også

For mer detaljer på datohåndtering i JavaScript, se følgende kilder:

1. Mozilla Developer Network's guide til Date objektet: https://developer.mozilla.org/no/docs/Web/JavaScript/Reference/Global_Objects/Date
2. JavaScript Date referanse på W3schools: https://www.w3schools.com/js/js_date_methods.asp