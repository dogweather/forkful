---
aliases:
- /nl/javascript/writing-to-standard-error/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:21.842935-07:00
description: "Schrijven naar de standaardfout (stderr) betekent tekst uitsturen naar\
  \ de foutenstroom. Het scheidt normale uitvoer (stdout) van fouten, wat debugging\
  \ en\u2026"
lastmod: 2024-02-18 23:09:02.294666
model: gpt-4-0125-preview
summary: "Schrijven naar de standaardfout (stderr) betekent tekst uitsturen naar de\
  \ foutenstroom. Het scheidt normale uitvoer (stdout) van fouten, wat debugging en\u2026"
title: Schrijven naar standaardfout
---

{{< edit_this_page >}}

## Wat & Waarom?
Schrijven naar de standaardfout (stderr) betekent tekst uitsturen naar de foutenstroom. Het scheidt normale uitvoer (stdout) van fouten, wat debugging en loganalyse gemakkelijker maakt.

## Hoe:

```javascript
// Een eenvoudige foutmelding naar stderr schrijven
console.error('Error: Er is iets misgegaan');

// Voorbeeld met geformatteerde uitvoer
const errorCode = 404;
console.error(`Fout: Pagina niet gevonden - Code ${errorCode}`);
```

Voorbeelduitvoer:
```
Error: Er is iets misgegaan
Fout: Pagina niet gevonden - Code 404
```

## Diepere Duik
Historisch gezien maken Unix-achtige systemen onderscheid tussen standaarduitvoer en standaardfout om het apart behandelen van reguliere berichten en foutberichten mogelijk te maken. Terwijl `console.log` in Javascript naar stdout schrijft, schrijft `console.error` specifiek naar stderr. Alternatieven voor het schrijven naar stderr zijn onder andere het gebruik van `process.stderr.write()`, wat geen nieuwe lijn karakter aan het einde toevoegt, in tegenstelling tot `console.error`.
Wat implementatie betreft, wanneer men Node.js scripts schrijft, kan de uitvoer naar `console.error()` gescheiden worden omgeleid van `console.log()` bij het uitvoeren van een script vanaf de opdrachtregel, wat handig kan zijn voor het loggen van fouten naar een ander bestand.

## Zie Ook
- MDN Web Docs over Console: https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Node.js documentatie over `process.stderr`: https://nodejs.org/api/process.html#process_process_stderr
- Uitleg over stdout vs stderr: https://www.jstor.org/stable/25860673
