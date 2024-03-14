---
date: 2024-01-26 00:54:40.212050-07:00
description: "Felhantering \xE4r hur du hanterar n\xE4r saker g\xE5r snett i din kod.\
  \ Det \xE4r viktigt eftersom det hj\xE4lper dina program att misslyckas anst\xE4\
  ndigt och instruerar\u2026"
lastmod: '2024-03-13T22:44:38.300745-06:00'
model: gpt-4-1106-preview
summary: "Felhantering \xE4r hur du hanterar n\xE4r saker g\xE5r snett i din kod.\
  \ Det \xE4r viktigt eftersom det hj\xE4lper dina program att misslyckas anst\xE4\
  ndigt och instruerar\u2026"
title: Hantering av fel
---

{{< edit_this_page >}}

## Vad och varför?

Felhantering är hur du hanterar när saker går snett i din kod. Det är viktigt eftersom det hjälper dina program att misslyckas anständigt och instruerar användare klart och tydligt, istället för att bara krascha och brinna upp.

## Hur man gör:

Här är den klassiska `try-catch`-blocket:

```javascript
try {
  // Kod som kanske kastar ett fel
  let result = potentiallyRiskyOperation();
  console.log('Lyckades:', result);
} catch (error) {
  // Vad man ska göra om ett fel kastas
  console.error('Hoppsan:', error.message);
}
```

Exempelutskrift när inget fel inträffar:
```
Lyckades: 42
```

Och när det är ett fel:
```
Hoppsan: Någonting gick fel
```

För asynkron kod, där löften (promises) är inblandade, använd `try-catch` i en `async`-funktion:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Data hämtad:', data);
  } catch (error) {
    console.error('Fel vid hämtning av data:', error.message);
  }
}

fetchData();
```

## Djupdykning

Felhantering i JavaScript har utvecklats. Förr i tiden (ES3, cirka 1999) hade vi bara `try-catch`-blocket. Inte superflexibelt, men det gjorde jobbet.

ES6 (2015) introducerade Löften (Promises) och gav oss `.then()` och `.catch()`, vilket tillät oss att hantera asynkrona fel mer smidigt.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Data hämtad:', data))
  .catch(error => console.error('Fel vid hämtning av data:', error.message));
```

När det gäller implementationsdetaljer, när ett fel kastas, skapar JavaScript-motorer ett `Error`-objekt med användbara egenskaper som `message` och `stack`. Du kan också göra anpassade feltyper genom att utöka `Error`-klassen – praktiskt för mer komplexa appar.

Alternativ? Du kan ignorera felhantering (dålig idé), använda funktioner med fel-först-parametrar (hej, Node.js-stil), eller bli flådigare med bibliotek och ramverk som erbjuder sina egna lösningar.

## Se även

För mer om felhantering:

- MDN om try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- En guide till Löften (Promises): [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Att skapa och kasta anpassade fel: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
