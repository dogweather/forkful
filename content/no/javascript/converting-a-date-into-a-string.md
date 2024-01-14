---
title:    "Javascript: Konvertere dato til streng"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng er en vanlig oppgave i Javascript programmering. Dette kan være nyttig for å presentere datoer til brukere på en mer forståelig måte, eller for å sammenligne datoer i en logikk som bruker strenger i stedet for dataobjekter.

## Hvordan
Det er flere måter å konvertere en dato til en streng i Javascript. Her er noen eksempler på hvordan du kan gjøre det:

```Javascript
// Konverterer en dato til en kort streng med bare dag og måned
var dato = new Date();
var streng = dato.toDateString(); // "Sun Oct 31 2021"

// Konverterer en dato til en lang streng med fullt år, måned og dag
var dato = new Date();
var streng = dato.toUTCString(); // "Sun, 31 Oct 2021 00:00:00 GMT"

// Konverterer en dato til en tilpasset streng med ønsket datoformat
var dato = new Date();
var datoformat = {weekday: 'long', year: 'numeric', month: 'long', day: 'numeric'}
var streng = dato.toLocaleDateString('nb-NO', datoformat); // "søndag 31. oktober 2021"
```

Det finnes også andre metoder for å konvertere en dato til en streng i Javascript. Det viktigste å huske på er å velge riktig metode basert på hva slags format og informasjon du ønsker å få ut av datoen.

## Deep Dive
Når du konverterer en dato til en streng, vil datoobjektet automatisk bruke tidssoneinnstillingene til enheten din. Dette kan være viktig å huske på hvis du ønsker å presentere datoer i et bestemt tidssoneformat. Du kan endre tidssonen ved å bruke metoden `toLocaleDateString()` og angi ønsket tidssone som en parameter.

Det kan være lurt å teste koden din på forskjellige enheter og nettlesere for å sikre at datoene blir presentert på samme måte uavhengig av enheten som blir brukt.

## Se også
- [Date Object in JavaScript](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [toLocaleDateString() Method in JavaScript](https://www.w3schools.com/jsref/jsref_tolocaledatestring.asp)
- [String Conversion in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)