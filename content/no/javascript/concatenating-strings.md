---
title:    "Javascript: Sammenføye strenger"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger er en vanlig oppgave i Javascript-programmering. Dette gjøres for å kombinere tekststrenger og variabler for å danne en lengre streng med informasjon. Dette kan være nyttig for å skrive ut tekster, eller for å bygge dynamiske nettapplikasjoner.

## Hvordan å gjøre det

For å kombinere strenger i javascript kan du bruke "+" operatøren til å binde strenger sammen.

```Javascript
let fornavn = "Ole";
let etternavn = "Hansen";
let fulltNavn = fornavn + " " + etternavn;
console.log(fulltNavn);
```

Output: "Ole Hansen"

I dette eksempelet har vi definert to variabler, "fornavn" og "etternavn", og deretter kombinert dem sammen ved hjelp av "+" -operatøren og lagret resultatet i en tredje variabel "fulltNavn". 

Du kan også bruke "+=" operatøren for å legge til en streng til en allerede eksisterende streng variabel.

```Javascript
let tekst = "Denne setningen ";
tekst += "fortsetter";
console.log(tekst);
```

Output: "Denne setningen fortsetter"

## Dykk ned

Et viktig poeng å merke seg er at når du kombinerer strenger med variabler, må variablene skrives ut med et pluss mellomrom før og etter variabelnavnet for å unngå at variabelen blir tolket som en del av strengen.

```Javascript
let antallEpler = 5;
console.log("Jeg spiste " + antallEpler + " epler til frokost.");
```

Output: "Jeg spiste 5 epler til frokost."

En annen nyttig funksjon for å kombinere strenger er forlengelsesmetoden .concat (). Dette kan brukes til å kombinere flere strenger sammen.

```Javascript
let tekst1 = "Jeg elsker ";
let tekst2 = "å programmere.";
let kombinertTekst = tekst1.concat(tekst2);
console.log(kombinertTekst);
```

Output: "Jeg elsker å programmere."

## Se også

For mer informasjon om hvordan du arbeider med strenger i Javascript, kan du se disse ressursene:

- [W3Schools: Strings in Javascript](https://www.w3schools.com/js/js_strings.asp)
- [MDN: Working with strings in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)