---
title:    "Javascript: Slette tegn som matcher et mønster"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et bestemt mønster er en viktig del av programmering, spesielt innenfor JavaScript. Dette kan være nyttig når man for eksempel ønsker å filtrere uønskede tegn i en streng eller behandle brukerinput på en sikker måte. Uten denne funksjonen ville det vært mye vanskeligere å manipulere tekst i en programmeringskontekst.

## Hvordan

For å slette tegn som matcher et mønster i JavaScript, kan man bruke en rekke forskjellige metoder. En av de mest effektive er ved å bruke den globale replace() metoden. Denne metoden tar to argumenter: et søkeuttrykk og et erstatningsuttrykk. For eksempel, hvis vi ønsker å slette alle tall fra en streng, kan vi bruke følgende kode:

```Javascript
let tekst = "Jeg har 5 bananer og 3 epler";
let nyTekst = tekst.replace(/[0-9]/g, "");
console.log(nyTekst);
```

I dette eksempelet bruker vi en regulær uttrykk, [0-9], for å finne alle tall i vår tekststreng. Den globale flagget (g) sørger for at alle forekomster av tegnene erstattes, ikke bare den første. Deretter bruker vi en tom streng som erstatning, slik at alle tallene slettes fra teksten. Resultatet av koden vil være:

```Javascript
Jeg har  bananer og  epler
````

Det finnes også andre metoder for å slette tegn som matcher et mønster i JavaScript, som for eksempel slice(), substring() og substr(). Men replace() er den mest fleksible og kraftfulle metoden for denne oppgaven.

## Dypdykk

Når man jobber med å slette tegn som matcher et mønster i JavaScript, er det viktig å være klar over forskjellen mellom et søkeuttrykk og et erstatningsuttrykk. Søkeuttrykket definerer hvilke tegn som skal slettes, mens erstatningsuttrykket definerer hva som skal erstatte disse tegnene. Det finnes også mange andre nyttige metoder og funksjoner som kan brukes sammen med replace() for å oppnå mer spesifikke resultater.

I tillegg er det verdt å merke seg at replace() returnerer en ny tekststreng og at den originale teksten ikke endres. Dette kan være viktig å huske på hvis man jobber med store mengder tekst og ønsker å unngå unødvendige ressurser og tidsbruk.

## Se også

- [MDN - String replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN - Regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools - String replace()](https://www.w3schools.com/jsref/jsref_replace.asp)