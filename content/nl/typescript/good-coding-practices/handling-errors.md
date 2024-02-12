---
title:                "Fouten afhandelen"
aliases:
- /nl/typescript/handling-errors.md
date:                  2024-01-28T22:02:10.379686-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Foutafhandeling is het verwachten van het onverwachte; het is hoe we omgaan met problemen in onze code. We doen het om crashes te voorkomen en om gebruikers een soepele ervaring te bieden, zelfs wanneer het onverwachte gebeurt.

## Hoe te:
In TypeScript omvat foutafhandeling vaak `try`, `catch` en `finally` blokken.

```typescript
function riskyOperation() {
  throw new Error("Er ging iets mis!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Er is een fout gevangen:", error.message);
  } finally {
    console.log("Dit wordt altijd uitgevoerd, fout of niet.");
  }
}

handleErrors();
```

Voorbeelduitvoer:

```
Er is een fout gevangen: Er ging iets mis!
Dit wordt altijd uitgevoerd, fout of niet.
```

Asynchroon voorbeeld met beloften:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Simuleer een fout
    reject("Jammerlijk gefaald");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Asynchroon fout gevangen:", error);
  }
}

handleAsyncErrors();
```

Voorbeelduitvoer:

```
Asynchroon fout gevangen: Jammerlijk gefaald
```

## Diepere Duik
Foutafhandeling is sinds het begin van het programmeren een hoeksteen geweest. In TypeScript, dat voortbouwt op JavaScript, werd foutafhandeling robuuster met de introductie van async/await in ECMAScript 2017. Daarvoor vertrouwden we vaak op callback-functies en beloften om fouten in asynchrone code te hanteren.

Een alternatief voor `try/catch` in TypeScript is het gebruik van foutbegrenzingen die door frameworks zoals React worden aangeboden. Voor serverzijdige afhandeling kunnen we middleware gebruiken op platforms zoals Express.js om foutbeheer te centraliseren.

Wat implementatie betreft, heeft TypeScript geen eigen foutafhandelingsmechanisme, maar vertrouwt het op JavaScript's. Aangepaste foutklassen kunnen de `Error`-klasse uitbreiden om meer beschrijvende foutinformatie te bieden.

## Zie Ook
- [MDN over try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await op MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Het gebruik van Error Grenzen in React](https://reactjs.org/docs/error-boundaries.html)
- [Express.js Foutafhandeling](https://expressjs.com/en/guide/error-handling.html)
