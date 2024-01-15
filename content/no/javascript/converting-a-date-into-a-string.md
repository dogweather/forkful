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

## Hvorfor

Å konvertere en dato til en tekststreng er en nyttig funksjon i JavaScript som lar deg vise datoer på en mer lesbar måte for brukerne dine. Det kan også være nyttig for å lagre datoer i et leselig format for databehandling. 

## Hvordan

For å konvertere en dato til en tekststreng i JavaScript, kan vi bruke metoden `toString()`. Dette vil returnere en streng av den spesifikke datoen som er formatert basert på datamaskinens lokale tidssone. La oss se på et eksempel:

```javascript
let date = new Date() // Oppretter en ny dato-objektet som representerer dagens dato
let dateString = date.toString() // Konverterer dato-objektet til en streng
console.log(dateString) // Output: "Fri Aug 20 2021 13:27:33 GMT+0200 (sentraleuropeisk sommertid)"
```

Vi kan også bruke metoden `toLocaleString()` for å få en mer lesevennlig datoformat basert på brukerens lokale tidssone. La oss se på et annet eksempel:

```javascript
let date = new Date()
let options = { dateStyle: "long", timeStyle: "short" }
let dateString = date.toLocaleString("nb-NO", options)
console.log(dateString) // Output: "20. august 2021 kl. 13:33"
```

Vi kan også formatere datoen manuelt ved å bruke metoder som `getDate()`, `getMonth()` og `getFullYear()` for å få informasjon om dag, måned og år. Deretter kan vi bruke disse verdiene sammen med tekststrenger for å bygge vår egen datoformat. La oss se på et eksempel:

```javascript
let date = new Date()
let day = date.getDate()
let month = date.getMonth() + 1 // Månedene starter på 0 i JavaScript, så vi må legge til 1
let year = date.getFullYear()
let dateString = day + "." + month + "." + year // Bygger vår egen datoformat
console.log(dateString) // Output: "20.8.2021"
```

## Dypdykk

Når vi bruker `toString()` eller `toLocaleString()` for å konvertere en dato til en tekststreng, vil den bli formatert basert på datamaskinens lokale tidssone. Dette betyr at samme JavaScript-kode kan gi forskjellige resultater avhengig av hvor og når den blir kjørt. Hvis du vil konvertere en dato til en tekststreng som alltid er i et spesifikt format, bør du bruke en bibliotek som Moment.js.

En annen ting å merke seg er at datoen i eksemplene våre vil bli returnert i engelsk format, selv om vi bruker `toLocaleString()` med en norsk lokalitet. Dette skyldes at standarddatoformatet i JavaScript er ISO 8601, som bruker engelske navn for måneder og ukedager. Hvis du vil ha en norsk datoformat, bør du bruke bibliotek som Moment.js eller formatere datoen manuelt som vist i eksempelet ovenfor.

## Se også

- [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Format a Date in JavaScript](https://www.w3schools.com/js/js_date_formats.asp)