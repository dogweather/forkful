---
title:                "Analysering av dato fra en streng"
html_title:           "TypeScript: Analysering av dato fra en streng"
simple_title:         "Analysering av dato fra en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å tolke en dato fra en tekst er når en programmerer tar en dato som er skrevet som tekst og konverterer den til et objekt som kan leses og manipuleres av datamaskinen. Dette er nyttig fordi det gjør det enklere å håndtere og analysere datoer i programmering, og kan hjelpe med å validere brukerinput.

## Hvordan:
```TypeScript
// Konverterer tekststreng til dato ved hjelp av Date ()-funksjonen
let date = new Date('May 26, 2020'); 

// Skriver ut dag, måned og år 
console.log(date.getDate());       // Output: 26
console.log(date.getMonth());      // Output: 4
console.log(date.getFullYear());   // Output: 2020 
```

## Dykke Dypt:
Parsing av datoer fra strenger har vært nyttig i programmering siden de tidlige dagene, da formaliserte datoformater og strukturerte datoobjekter ennå ikke var utviklet. Alternativene til å bruke Date ()-funksjonen inkluderer å bruke en tredjeparts bibliotek som moment.js eller å implementere tilpassede funksjoner for å tolke datoer fra spesifikke formater.

## Se også:
- [Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)