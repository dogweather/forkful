---
title:                "Å jobbe med json"
html_title:           "TypeScript: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du programmerer i TypeScript, vil du sannsynligvis komme over JSON-data på et tidspunkt. JSON er et populært format for å lagre og overføre data, og å kunne jobbe med det i TypeScript vil være en nyttig ferdighet å ha.

## Hvordan

For å jobbe med JSON i TypeScript, er det først og fremst viktig å huske på at JSON-data representeres som JavaScript-objekter. Dette betyr at du kan bruke alle de vanlige objektorienterte metodene og egenskapene du allerede er kjent med. Her er et enkelt eksempel på hvordan du kan konvertere et JavaScript-objekt til JSON og vice versa:

```TypeScript
// Konverterer et JavaScript-objekt til JSON
let person = {navn: "Ole", alder: 30};
let personJSON = JSON.stringify(person);
console.log(personJSON); // {"navn": "Ole", "alder":30}

// Konverterer JSON til et JavaScript-objekt
let personObjekt = JSON.parse(personJSON);
console.log(personObjekt.navn); // "Ole"
console.log(personObjekt.alder); // 30
```

Som du kan se, bruker vi JSON-objektet for å konvertere mellom JavaScript-objekt og JSON-data. Det er viktig å merke seg at JSON ikke kan lagre funksjoner, så når du bruker `JSON.stringify()` vil alle funksjoner i objektet bli utelatt.

En annen nyttig måte å håndtere JSON-data i TypeScript er å bruke et bibliotek som heter `json2typescript`. Dette biblioteket lar deg definere TypeScript-klasser som samsvarer med JSON-strukturen du jobber med. Så kan du enkelt konvertere JSON-data til TypeScript-objekter og til og med validere strukturen av dataene dine. Her er et eksempel på hvordan du bruker `json2typescript` for å konvertere JSON-data til TypeScript:

```TypeScript
// Sett opp en TypeScript-klasse som samsvarer med JSON-strukturen
class Person {
  public navn: string;
  public alder: number;
}

// Opprett en instans av Person-klassen
let person = new Person();
person.navn = "Ole";
person.alder = 30;

// Konverterer JavaScript-objekt til JSON
let personJSON = JSON.stringify(person);

// Bruk json2typescript for å konvertere JSON tilbake til et TypeScript-objekt
let personObjekt = JsonSerializer.deserialize<Person>(personJSON, Person);

console.log(personObjekt.navn); // "Ole"
console.log(personObjekt.alder); // 30
```

## Deep Dive

En viktig ting å huske på når du jobber med JSON i TypeScript er hvordan du håndterer eventuelle unødvendige felter eller verdier. Hvis du bruker `JSON.stringify()` på et JavaScript-objekt som inneholder et null-verdi eller et tomt array, vil disse feltene bli utelatt i den resulterende JSON-strengen. Når du deretter bruker `JSON.parse()` for å konvertere JSON tilbake til et JavaScript-objekt, vil disse feltene være fraværende siden de ikke ble inkludert i JSON-dataen.

Det er et par måter å håndtere dette på. Du kan enten bruke `||` operatøren og sette en standardverdi for felter som kan være null, eller du kan bruke `Object.prototype.hasOwnProperty()` for å sjekke om et felt eksisterer før du bruker det. For mer informasjon om dette, sjekk ut denne artikkelen på Stack Overflow.

## Se Også

- [TypeScript Offisiell Dokumentasjon](https://www.typescriptlang.org/docs/)
- [json2typescript på GitHub](https://github.com/Hotell/json2typescript)
- [Stack Overflow: Slik håndterer du felter som kan være null i JSON](https://stackoverflow.com/questions/286141/remove-blank-attributes-from-an-object-in-javascript)