---
title:    "Javascript: Konvertering av dato til en streng"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en dato til en streng er en vanlig oppgave innen JavaScript-programmering. Datoer er en viktig del av mange applikasjoner, og det er viktig å kunne presentere dem på en lesbar måte for brukere. I denne bloggposten vil vi gå gjennom hvorfor og hvordan man konverterer en dato til en streng i JavaScript.

## Slik gjør du det

Det finnes flere måter å konvertere en dato til en streng på i JavaScript. En enkel måte å gjøre det på er å bruke to innebygde metoder: `toLocaleDateString()` og `toLocaleTimeString()`. Disse metodene tar inn et dato-objekt og returnerer en lokal tid/dato på formatet som er satt i operativsystemets språkinnstillinger. La oss se på et eksempel:

```Javascript
let dato = new Date(); // Oppretter et nytt dato-objekt med dagens dato
let datoTekst = dato.toLocaleDateString() + " " + dato.toLocaleTimeString(); // Konverterer til dato og tid på formatet "dd.mm.åååå tt:nn:ss"
console.log(datoTekst); // Output: 22.04.2021 12:30:00
```

Dette er en enkel metode som fungerer i de fleste tilfeller. Om du derimot ønsker å ha full kontroll over hvordan datoen blir formatert, kan du bruke `getDate()`, `getMonth()` og `getFullYear()`-metodene for å hente ut dag, måned og år fra et dato-objekt. Deretter kan du sette sammen disse verdiene på ønsket format ved hjelp av for eksempel `String.padStart()`-metoden for å legge til nuller foran tall som er mindre enn 10. La oss se på et eksempel:

```Javascript
let dato = new Date(); // Oppretter et nytt dato-objekt med dagens dato
let datoTekst = dato.getDate().padStart(2, '0') + "." + (dato.getMonth()+1).padStart(2, '0') + "." + dato.getFullYear(); // Konverterer til dato på formatet "dd.mm.åååå"
console.log(datoTekst); // Output: 22.04.2021
```

Som du kan se, bruker vi `padStart()`-metoden for å legge til en null foran tall mindre enn 10, sånn at måneder og dager er på to-sifret format.

## Dypdykk

Når du konverterer en dato til en streng i JavaScript, kan det være viktig å tenke på hvilke formater som er akseptable for ulike brukere. I Norge bruker vi for eksempel ofte dmy-formatet (dag.måned.år), mens i andre land brukes mdy-formatet (måned/dag/år). Derfor er det viktig å være bevisst på hvilken metode man bruker for å konvertere datoer, og eventuelt tilpasse seg til ulike formater basert på brukerens lokasjon.

Det er også viktig å være oppmerksom på at når man konverterer en dato til en streng, mister man informasjon om tidssone. Dette kan være problematisk i noen tilfeller, og derfor må man være forsiktig med hvordan man bruker datoen videre i applikasjonen.

## Se også

- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getDate
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/padStart