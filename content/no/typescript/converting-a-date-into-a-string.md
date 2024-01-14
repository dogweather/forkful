---
title:    "TypeScript: Konvertere en dato til en streng"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Det kan være nødvendig å konvertere datoer til strenger i en TypeScript-applikasjon for å presentere dem på en mer lesbar og forståelig måte for brukeren. Dette kan ofte være nyttig når man arbeider med data som inneholder datofelt, for eksempel i et fakturasystem.

## Hvordan

For å konvertere en dato til en streng i TypeScript, kan man bruke den innebygde `toLocaleString()`-metoden. Denne metoden tar inn et språkparameter som argument, og returnerer en lesbar tekst i henhold til det valgte språket.

```TypeScript
const today = new Date(); // Oppretter et nytt Date-objekt for dagens dato
const formattedDate = today.toLocaleString('nb-NO'); // Konverterer til en lesbar tekst på norsk
console.log(formattedDate); // Output: "18. august 2021, 11:05:00"
```

Dette eksempelet viser hvordan man kan konvertere en dato til en lesbar streng på norsk ved hjelp av `toLocaleString()`-metoden.

## Dypdykk

Ved bruk av `toLocaleString()`-metoden kan man også formatere datoen på ulike måter, for eksempel å endre rekkefølgen på dag, måned og år, eller legge til timer og minutter for å presentere en klokkeslett. Man kan også spesifisere hvilket språk man ønsker å bruke, slik at datoen blir presentert i henhold til det valgte språket.

Det finnes også andre metoder for å konvertere en dato til en streng i TypeScript, som for eksempel `toDateString()` og `toISOString()`. Du kan lese mer om disse metodene og deres funksjoner i TypeScript-dokumentasjonen.

## Se også

* [TypeScript Dokumentasjon](https://www.typescriptlang.org/docs/)
* [Mozilla Developer Network - Date.prototype.toLocaleString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)