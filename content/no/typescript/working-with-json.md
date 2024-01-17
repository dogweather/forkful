---
title:                "Å arbeide med json"
html_title:           "TypeScript: Å arbeide med json"
simple_title:         "Å arbeide med json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med JSON er å behandle data i en lettleselig, tekstbasert format som brukes ofte i webprogrammering. Programmere bruker JSON for å lagre og overføre data mellom forskjellige språk og systemer enkelt og effektivt.

## Hvordan:

Enkelt eksempel på å skrive en JSON-streng i TypeScript og deretter parse den tilbake:

```
let jsonStreng = '{"navn": "Erik", "alder": 27}';

let person = JSON.parse(jsonStreng);

console.log(person.navn); // utskrift: Erik
console.log(person.alder); // utskrift: 27
```

## Dypdykk:

JSON (JavaScript Object Notation) ble utviklet i 1990-årene som en enkel måte for programmerere å utveksle strukturert data mellom forskjellige programmeringsspråk. Alternativer til JSON inkluderer XML og CSV, men JSON har blitt det foretrukne valget for mange på grunn av sin enkelhet og leselighet. I TypeScript brukes det ofte i form av JSON-strenger, men det finnes også innebygde funksjoner for å arbeide med JSON-objekter.

## Se også:

- TypeScript-dokumentasjon: https://www.typescriptlang.org/docs/handbook/basic-types.html
- JSON.org: https://www.json.org/json-en.html