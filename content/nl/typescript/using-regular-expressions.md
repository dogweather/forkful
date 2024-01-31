---
title:                "Reguliere expressies gebruiken"
date:                  2024-01-28T22:10:08.774994-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies, of regex, is een krachtig patroonzoek- en -matchinghulpmiddel in programmeren. Programmeurs gebruiken regex voor taken zoals het valideren van gebruikersinvoer, het doorzoeken van tekst, of het manipuleren van strings omdat het efficiënt en veelzijdig is.

## Hoe te:

Laten we duiken in TypeScript en zien hoe regex wordt gebruikt voor veelvoorkomende taken.

```TypeScript
// Definieer een regex patroon voor een e-mailadres
const emailPattern = /\S+@\S+\.\S+/;

// Test of een string overeenkomt met het e-mailpatroon
const email = "gebruiker@voorbeeld.com";
console.log(emailPattern.test(email)); // Uitvoer: true

// Vind en vervang cijfers in een string
const replaceDigits = "Item 25 kost $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Uitvoer: "Item # kost $#"

// Specifieke delen uit een string extraheren met behulp van vanggroepen
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, maand, dag, jaar] = datePattern.exec(data) || [];
console.log(maand, dag, jaar); // Uitvoer: "April" "10" "2021"
```

## Diepgaand

Terug in de jaren 1950 beschreef wiskundige Stephen Kleene reguliere expressies als een model om reguliere talen te vertegenwoordigen, wat later essentieel werd in de informatica. Fast forward, regex is alomtegenwoordig in programmeren voor het omgaan met tekst.

Hoewel regex een Zwitsers zakmes is voor stringbewerkingen, is het niet zonder alternatieven. Afhankelijk van de complexiteit van de taak, kunnen soms stringmethoden zoals `includes()`, `startsWith()`, `endsWith()`, of zelfs parseren met een bibliotheek beter zijn. Bijvoorbeeld, het parsen van een complexe JSON-string met regex kan een nachtmerrie zijn - gebruik in plaats daarvan een JSON-parser.

Wat betreft de implementatie, regex in JavaScript en TypeScript is gebaseerd op de ECMAScript taalspecificatie. Onder de motorkap gebruiken engines staatmachines om efficiënt patronen te matchen. Het is de moeite waard om te noteren dat regex-operaties duur kunnen worden in termen van prestaties, vooral met slecht geschreven patronen - let op voor "catastrofale terugvolging".

## Zie Ook

- MDN Web Docs over reguliere expressies: [MDN Reguliere Expressies](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: Een hulpmiddel om regex patronen te testen en debuggen [Regex101](https://regex101.com/)
- "Mastering Regular Expressions" boek voor diepgaand begrip: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
