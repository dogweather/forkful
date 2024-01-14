---
title:    "Javascript: Utvinning av delstrenger"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å utvinne substringer kan være nyttig i mange forskjellige programmeringsscenarioer. Det kan hjelpe deg å manipulere tekst på en mer effektiv måte, for eksempel ved å fjerne uønskede deler av en streng eller få tak i spesifikke deler av en streng.

## Hvordan du gjør det

```Javascript
// Eksempel på å utvinne en del av en streng
const navn = "Johan Olsen";
const etternavn = navn.substring(6);
console.log(etternavn); // Output: Olsen

// Eksempel på å utvinne en del av en streng fra en bestemt indeks
const setning = "Denne artikkelen er skrevet på norsk.";
const del = setning.substring(17,22);
console.log(del); // Output: norsk
```

I disse eksemplene brukte vi metoden `substring` på en streng og anga hvilke deler av strengen vi ønsket å utvinne. Den første parameteren er startindeksen, mens den andre er sluttposisjonen. Hvis ingen sluttposisjon er angitt, vil metoden utvinne resten av strengen fra startindeksen.

Du kan også bruke en lignende metode, `slice`, som også tar en start- og sluttposisjon som argumenter, men som også lar deg bruke negative tall for å begynne fra slutten av strengen.

```Javascript
// Eksempel på å bruke slice med negative tall
const navn = "Maria Hernandez";
const etternavn = navn.slice(-9);
console.log(etternavn); // Output: Hernandez

// Eksempel på å bruke slice med negative tall for å utvinne en del av en streng fra slutten
const setning = "Denne artikkelen er skrevet på norsk.";
const del = setning.slice(-6,-1);
console.log(del); // Output: norsk
```

Begge disse metodene kan også brukes på arrays for å utvinne deler av dem.

## Dypdykk

Det er også verdt å nevne at det finnes flere andre metoder for å utvinne substringer, for eksempel `substr` og `split`. Det er også mulig å bruke regulære uttrykk for å utvinne deler av en streng.

Husk at i Javascript, så er strenger immutable (kan ikke endres), så utvinning av substringer vil alltid gi deg en ny streng, istedenfor å endre den opprinnelige.

## Se også

- [MDN dokumentasjon om substring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN dokumentasjon om slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN dokumentasjon om substr](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN dokumentasjon om split](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)