---
title:                "Organisering av kode i funksjoner"
aliases:
- /no/javascript/organizing-code-into-functions.md
date:                  2024-01-26T01:11:08.478503-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner bryter opp oppgaver i gjenbrukbare deler, noe som gjør koden ryddigere og lettere å vedlikeholde. Vi gjør dette for å redusere redundans, gjøre testing enklere, og forbedre lesbarheten.

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
