---
title:                "Fouten afhandelen"
date:                  2024-01-28T22:01:39.786275-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

category:             "Javascript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Foutafhandeling is hoe je beheert wat er gebeurt wanneer er iets misgaat in je code. Het is essentieel omdat het ervoor zorgt dat je programma's op een elegante manier falen en duidelijke instructies geven aan gebruikers, in plaats van gewoonweg te crashen en te branden.

## Hoe:

Hier is het klassieke `try-catch` blok:

```javascript
try {
  // Code die een fout kan veroorzaken
  let resultaat = potentieelRisicovolleOperatie();
  console.log('Succes:', resultaat);
} catch (fout) {
  // Wat te doen als een fout optreedt
  console.error('Oeps:', fout.message);
}
```

Voorbeelduitvoer wanneer er geen fout optreedt:
```
Succes: 42
```

En wanneer er een fout optreedt:
```
Oeps: Er is iets fout gegaan
```

Voor asynchrone code, waar promises bij betrokken zijn, gebruik `try-catch` in een `async` functie:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Data opgehaald:', data);
  } catch (fout) {
    console.error('Fout bij het ophalen van gegevens:', fout.message);
  }
}

fetchData();
```

## Diepere Duik

Foutafhandeling in JavaScript is geëvolueerd. Terug in de dag (ES3, circa 1999), hadden we alleen het `try-catch` blok. Niet super flexibel, maar het deed het werk.

ES6 (2015) introduceerde Promises en gaf ons `.then()` en `.catch()`, waardoor we asynchrone fouten eleganter konden afhandelen.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Data opgehaald:', data))
  .catch(fout => console.error('Fout bij het ophalen van gegevens:', fout.message));
```

Wat betreft implementatiedetails, wanneer een fout wordt veroorzaakt, creëren JavaScript-engines een `Error` object met nuttige eigenschappen zoals `message` en `stack`. Je kunt ook aangepaste fouttypes maken door de `Error` klasse uit te breiden – handig voor complexere apps.

Alternatieven? Je zou foutafhandeling kunnen negeren (slecht idee), callbacks gebruiken met fout-eerste parameters (hallo, Node.js stijl), of het mooier maken met bibliotheken en frameworks die hun eigen interpretaties bieden.

## Zie Ook

Voor meer over foutafhandeling:

- MDN over try-catch: [MDN try...catch](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Statements/async_function)
- Een gids voor Promises: [MDN Promises](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Het aanmaken en gooien van aangepaste fouten: [MDN Fout](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/Error)
