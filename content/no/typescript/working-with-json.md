---
title:                "TypeScript: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å arbeide med JSON kan være svært nyttig for alle som er interessert i å jobbe med datauttveksling og nettsider. JSON er et enkelt og fleksibelt format som lar deg lagre og overføre data på en effektiv måte. Det er også et populært format innenfor webutvikling og brukes til å kommunisere data mellom klient og server.

## Slik gjør du det 

Så hvordan kan du jobbe med JSON i TypeScript? Det første du må gjøre er å importere json-modulen i prosjektet ditt. Dette gjør du ved å legge til følgende linje med kode øverst i TypeScript-filen din:

```TypeScript
import * as jsonData from './data.json'; 
```

Nå kan du lese dataene i JSON-filen ved å bruke variabelen `jsonData` som ble opprettet i importen. Du kan også filtrere og manipulere dataene etter behov. For eksempel kan du skrive ut alle elementene i et array ved å bruke en løkke:

```TypeScript
for (let element of jsonData) {
  console.log(element); 
}
```

Du kan også bruke et objekt innenfor json-modulen til å konvertere dataene fra JSON til TypeScript objekter. Dette gjøres ved å bruke `parse` funksjonen på følgende måte:

```TypeScript
let jsonStr = '{"name":"John", "age":30, "city":"New York"}';
let obj = JSON.parse(jsonStr); 
```

## Dypdykk 

Når du jobber med JSON i TypeScript er det viktig å være oppmerksom på datatypene. JSON støtter bare en håndfull typer som tall, strenger, boolean og null. Dette kan føre til problemer hvis du for eksempel prøver å lese et ugyldig tall eller en ugyldig datatype fra en JSON-fil.

Det kan også være nyttig å lære mer om hvordan du håndterer feil og unntak når du jobber med JSON. TypeScript har innebygde metoder for å håndtere disse situasjonene, som for eksempel `try/catch` konstruksjonen.

## Se også 

- ["Understanding JSON in TypeScript"](https://medium.com/javascript-in-plain-english/understanding-json-in-typescript-335d99313e81)
- ["JSON in TypeScript: A Beginner's Guide"](https://www.valentinog.com/blog/json/)
- ["Manipulating JSON data with TypeScript"](https://dev.to/idiglo/manipulating-json-data-with-typescript-d83)