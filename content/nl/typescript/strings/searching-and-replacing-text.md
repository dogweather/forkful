---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:25.807459-07:00
description: "Tekst zoeken en vervangen in strings is een veelvoorkomende taak in\
  \ programmering, vaak gebruikt om gegevens te verwerken en te manipuleren. Het is\u2026"
lastmod: '2024-03-13T22:44:50.534488-06:00'
model: gpt-4-0125-preview
summary: Tekst zoeken en vervangen in strings is een veelvoorkomende taak in programmering,
  vaak gebruikt om gegevens te verwerken en te manipuleren.
title: Tekst zoeken en vervangen
weight: 10
---

## Hoe te:
TypeScript, gebouwd op JavaScript, komt met handige methoden voor stringmanipulatie. We kunnen `String.prototype.replace()` gebruiken voor basis zoek- en vervangbewerkingen. Bekijk deze snippets:

```typescript
// Eenvoudige string vervanging
let text: string = "Hallo, Wereld!";
let newText: string = text.replace("Wereld", "TypeScript");
console.log(newText);  // Uitvoer: Hallo, TypeScript!

// Globale vervanging met regex
let regexText: string = "foo bar foo bar";
let globalRegex: RegExp = /foo/g;
let newRegexText: string = regexText.replace(globalRegex, "baz");
console.log(newRegexText);  // Uitvoer: baz bar baz bar

// Vervangen met een functie
let dynamicText: string = "Ik heb 2 appels en 5 sinaasappels.";
let fruitCounter: string = dynamicText.replace(/\d+/g, (match) => {
    return (+match * 2).toString();
});
console.log(fruitCounter);  // Uitvoer: Ik heb 4 appels en 10 sinaasappels.
```

## Dieper Duiken
Historisch gezien is tekstvervanging een functie geweest in zelfs de vroegste tekstverwerkingshulpmiddelen, met Unix-tools zoals `sed` als iconische voorbeelden. In modernere programmering zijn vervangingsoperaties vaak krachtiger wanneer ze worden gekoppeld aan reguliere expressies (regex) voor patroonherkenning.

Alternatieven voor `String.prototype.replace()` in TypeScript zijn talrijk. Bibliotheken zoals Lodash bieden `_.replace()` met een vergelijkbare syntax. Voor meer geavanceerde scenario's, zou je kunnen overwegen om je eigen parser te bouwen of parser-bibliotheken te gebruiken voor transformatietaken die verder gaan dan eenvoudige stringvervanging.

Als we het over implementatie hebben, onthoud dan dat `.replace()` de originele string niet zal muteren. Strings in JavaScript en TypeScript zijn onveranderlijk. De methode retourneert een nieuwe string, dus als je de aangepaste tekst nodig hebt, zul je deze moeten opslaan, zoals in de bovenstaande voorbeelden.

## Zie Ook
- MDN Web Docs over `replace()`: [MDN String vervangen](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Regex-testtool om je vaardigheden in patroonherkenning aan te scherpen: [Regex101](https://regex101.com/)
- Lodash's string vervangen voor een alternatieve benadering: [Lodash _.replace](https://lodash.com/docs/4.17.15#replace)
