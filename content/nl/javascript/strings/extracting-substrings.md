---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:40.419628-07:00
description: "Het extraheren van substrings betekent het grijpen van een stukje van\
  \ een string. Programmeurs snijden en hakken in strings om data te isoleren, een\u2026"
lastmod: '2024-03-13T22:44:51.189538-06:00'
model: gpt-4-0125-preview
summary: Het extraheren van substrings betekent het grijpen van een stukje van een
  string.
title: Substrings extraheren
weight: 6
---

## Wat & Waarom?
Het extraheren van substrings betekent het grijpen van een stukje van een string. Programmeurs snijden en hakken in strings om data te isoleren, een bepaald formaat in te voeren, of tekst aan te passen voor output.

## Hoe te:

### Gebruikmakend van de `substring` methode:
```javascript
let tekst = "JavaScript is geweldig!";
let geextraheerd = tekst.substring(0, 10);
console.log(geextraheerd); // Uitvoer: JavaScript
```

### Gebruikmakend van de `slice` methode:
```javascript
let tekst = "JavaScript is geweldig!";
let gesneden = tekst.slice(-9, -1);
console.log(gesneden); // Uitvoer: geweldig
```

### Gebruikmakend van de `substr` methode (verouderd):
```javascript
let tekst = "JavaScript is geweldig!";
let substrd = tekst.substr(11, 7);
console.log(substrd); // Uitvoer: geweldig
```

## Diepgaande Duik
Het extraheren van substrings is niet nieuw – het is zo oud als programmeren zelf. De `substring` en `slice` methoden in JavaScript zijn gereedschappen uit de jaren 90, deel van de initiële kenmerkenset van de taal. `substr` was er ook, maar is nu legacy code en zou vermeden moeten worden in moderne applicaties.

Het verschil? `substring` en `slice` zijn vergelijkbaar – beide nemen start- en eindindex parameters – maar gaan anders om met negatieven: `slice` kan negatieve indices aan, die tellen vanaf het einde, terwijl `substring` ze als nullen behandelt. Al deze methoden muteren de oorspronkelijke string niet; ze produceren nieuwe.

## Zie ook
- Mozilla Developer Network over Strings: [MDN Web Docs - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Stringmanipulatie met JavaScript: [W3Scholen - JavaScript String Methoden](https://www.w3schools.com/js/js_string_methods.asp)
- Basiskennis over JavaScript-strings: [JavaScript.info - Strings](https://javascript.info/string)
