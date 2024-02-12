---
title:                "Tekst zoeken en vervangen"
aliases:
- /nl/javascript/searching-and-replacing-text/
date:                  2024-01-28T22:07:07.201559-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Tekst zoeken en vervangen betekent het vinden van specifieke substrings en deze omwisselen voor iets nieuws. Waarom zou je je hiermee bezighouden? Het is overal: typfouten in een document corrigeren, code verfijnen of gegevens in batch bewerken.

## Hoe:
In JavaScript is `String.prototype.replace()` de methode bij uitstek. Geef een string of regex en de vervanging op. Hier is de snelle en vuile manier:

```javascript
let str = "Ik hou ervan om in JavaScript te coderen!";
let newStr = str.replace("JavaScript", "TypeScript");
console.log(newStr); // Geeft uit: Ik hou ervan om in TypeScript te coderen!
```

Nu, met regex voor globale vervangingen:

```javascript
let verhaal = "De snelle bruine vos springt over de luie hond. De vos is slim.";
let nieuwVerhaal = verhaal.replace(/vos/g, "kat");
console.log(nieuwVerhaal); // Geeft uit: De snelle bruine kat springt over de luie hond. De kat is slim.
```

## Diep Duiken
Terugkijkend, `String.prototype.replace()` zit al in JS sinds de vroege dagen—Netscape 2 vroeg. Nu heeft ES6 ons sjabloontechnieken en pijlfuncties gebracht, die de zaak opgekruide met meer beknopte en leesbare code met regex.

Alternatieven? Zeker. Als je met grootschalige tekstverwerking werkt, kun je uitwijken naar Node.js streams of externe bibliotheken gebruiken om complexe patronen, efficiëntie en prestaties aan te pakken.

Wat betreft de implementatie, `replace()` op zich is simpel. Maar regex patronen kunnen wild worden. Begin makkelijk, leer de speciale karakters (`.` komt overeen met elk teken, `*` voor herhalende patronen), en test met tools zoals regex101.

## Zie Ook
- MDN replace documentatie: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 voor het testen van uitdrukkingen: https://regex101.com/
- JavaScript info over regex: https://javascript.info/regexp-introduction
