---
date: 2024-01-26 01:11:08.478503-07:00
description: "Hvordan gj\xF8re det: Historisk sett manglet imperative programmeringsspr\xE5\
  k som tidlige versjoner av BASIC eller Assembly abstraksjonen som funksjoner\u2026"
lastmod: '2024-04-05T22:50:55.196461-06:00'
model: gpt-4-1106-preview
summary: "Historisk sett manglet imperative programmeringsspr\xE5k som tidlige versjoner\
  \ av BASIC eller Assembly abstraksjonen som funksjoner tilbyr."
title: Organisering av kode i funksjoner
weight: 18
---

## Hvordan gjøre det:
```javascript
// Definer en funksjon for å beregne arealet av et rektangel
function calculateArea(width, height) {
  return width * height;
}

// Kall på funksjonen og skriv ut resultatet
let area = calculateArea(5, 3);
console.log(area); // Utdata: 15
```

```javascript
// Grupper relatert funksjonalitet ved hjelp av funksjoner
function greet(name) {
  console.log(`Hei, ${name}!`);
}

function farewell(name) {
  console.log(`Ha det, ${name}!`);
}

greet('Alice'); // Utdata: Hei, Alice!
farewell('Bob'); // Utdata: Ha det, Bob!
```

## Dypdykk
Historisk sett manglet imperative programmeringsspråk som tidlige versjoner av BASIC eller Assembly abstraksjonen som funksjoner tilbyr. Over tid introduserte konseptet med modulær kode i språk som C ideen om at det å bryte ned kode i enheter (funksjoner eller prosedyrer) fører til bedre organisering og klarere logikk.

I JavaScript har vi, i tillegg til rene funksjoner, hatt pillefunksjoner siden ES6 (2015), som gir en mer konsis syntaks og er egnet for ikke-metodefunksjoner.

Alternativer og forbedringer rundt organisering av kode i JavaScript inkluderer objektorienterte tilnærminger ved bruk av klasser, eller funksjonelle programmeringsparadigmer som behandler funksjoner som førsteklasses borgere.

Når det gjelder implementering, støtter JavaScriptfunksjoner lukninger (closures), som gir en måte å beholde tilgang til en funksjons skope etter utførelse, noe som er kraftig for inkapsling og opprettelse av fabrikkfunksjoner, blant andre mønstre.

## Se også
- MDN Web Docs om Funksjoner: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- JavaScript Designmønstre: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Ren kode JavaScript: https://github.com/ryanmcdermott/clean-code-javascript
