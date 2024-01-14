---
title:    "Javascript: Utskrift av feilsøkingsutdata"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor

Feilsøking er en viktig del av enhver programmeringsoppgave, spesielt når det kommer til større og mer komplekse prosjekter. Å skrive ut debug-utdata kan hjelpe deg med å identifisere feil og forstå hvordan koden din fungerer. Det kan også være nyttig for å verifisere at koden din gjør det du forventer den skal gjøre, spesielt når du implementerer nye funksjoner eller gjør endringer.

## Hvordan du gjør det

For å skrive ut debug-utdata i JavaScript, kan du bruke console.log()-funksjonen. Du kan skrive hva som helst i parantesen og det vil bli skrevet ut i konsollen. La oss ta en titt på et enkelt eksempel:

```
let tall = 5;
console.log(tall); // skriver ut verdien av variabelen "tall" til konsollen
```

I dette eksempelet vil verdien av variabelen "tall" (5) bli skrevet ut i konsollen når programmet kjører. Dette kan være nyttig når du vil se på verdien av en variabel som endres gjennom koden din.

Du kan også inkludere informasjon om hvor i koden din debug-utdataen kommer fra ved å bruke console.log() sammen med en streng:

```
console.log("Feilsøking av variabelen tall: ", tall);
```

Dette vil skrive ut "Feilsøking av variabelen tall: 5" i konsollen. Det gir deg mer kontekst rundt debug-utdataen og kan hjelpe deg med å forstå hva som skjer i koden din.

## Dypdykk

Det finnes også andre metoder for å skrive ut debug-utdata, som console.error() og console.warn(), som hjelper deg med å identifisere forskjellige typer feil i koden din. Du kan også bruke console.table() for å skrive ut datastrukturer som objekter og arrayer på en mer organisert måte.

Det er viktig å huske å fjerne eller kommentere ut alle debug-utdatat hvis du er ferdig med feilsøking, da det kan påvirke ytelsen til koden din.

## Se også

- [JavaScript console API](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Debugging in JavaScript](https://www.w3schools.com/js/js_debugging.asp)
- [Mastering the Developer Tools Console](https://medium.com/@nickytonline/mastering-the-developer-tools-console-dfb2e88084df)