---
date: 2024-01-26 01:11:18.380176-07:00
description: "Hur man g\xF6r: Historiskt sett saknade imperativa programmeringsspr\xE5\
  k som tidiga versioner av BASIC eller Assembly den abstraktion som funktioner\u2026"
lastmod: '2024-04-05T21:53:39.636212-06:00'
model: gpt-4-1106-preview
summary: "Historiskt sett saknade imperativa programmeringsspr\xE5k som tidiga versioner\
  \ av BASIC eller Assembly den abstraktion som funktioner tillhandah\xE5ller."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
```javascript
// Definiera en funktion för att beräkna arean av en rektangel
function calculateArea(bredd, hojd) {
  return bredd * hojd;
}

// Anropa funktionen och skriv ut resultatet
let area = calculateArea(5, 3);
console.log(area); // Utdata: 15
```

```javascript
// Gruppera relaterad funktionalitet med funktioner
function greet(namn) {
  console.log(`Hej, ${namn}!`);
}

function farewell(namn) {
  console.log(`Adjö, ${namn}!`);
}

greet('Alice'); // Utdata: Hej, Alice!
farewell('Bob'); // Utdata: Adjö, Bob!
```

## Fördjupning
Historiskt sett saknade imperativa programmeringsspråk som tidiga versioner av BASIC eller Assembly den abstraktion som funktioner tillhandahåller. Över tid introducerade konceptet med modulär kod i språk som C idén om att bryta ner kod i enheter (funktioner eller procedurer) vilket leder till bättre organisation och tydligare logik.

I JavaScript har vi, förutom vanliga funktioner, också pilsyntax-funktioner sedan ES6 (2015) som erbjuder en mer koncis syntax och är lämpliga för icke-metodfunktioner.

Alternativ och förbättringar runt att organisera kod i JavaScript inkluderar objektorienterade tillvägagångssätt som använder klasser, eller funktionella programmeringsparadigm som behandlar funktioner som förstaklassens medborgare.

När det gäller genomförandet stödjer JavaScript-funktioner stängningar, vilket ger ett sätt att behålla åtkomst till en funktions omfång efter exekvering, vilket är kraftfullt för inkapsling och skapande av fabriksfunktioner, bland andra mönster.

## Se också
- MDN Web Docs om Funktioner: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- JavaScript Designmönster: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Clean Code JavaScript: https://github.com/ryanmcdermott/clean-code-javascript
