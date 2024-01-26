---
title:                "Hantering av fel"
date:                  2024-01-26T00:58:40.021875-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hantering av fel"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/handling-errors.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hantera fel handlar om att förvänta sig det oväntade; det är hur vi hanterar när saker går fel i vår kod. Vi gör det för att undvika krascher och för att ge användarna en smidig upplevelse, även när det oväntade inträffar.

## Hur man gör:
I TypeScript involverar hantering av fel ofta `try`, `catch` och `finally` block.

```typescript
function riskyOperation() {
  throw new Error("Något gick fel!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Fångade ett fel:", error.message);
  } finally {
    console.log("Detta körs alltid, fel eller inte.");
  }
}

handleErrors();
```

Exempel på utskrift:

```
Fångade ett fel: Något gick fel!
Detta körs alltid, fel eller inte.
```

Asynkront exempel med löften:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Simulera ett fel
    reject("Misslyckades miserabelt");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Fångade asynkront fel:", error);
  }
}

handleAsyncErrors();
```

Exempel på utskrift:

```
Fångade asynkront fel: Misslyckades miserabelt
```

## Fördjupning
Felhantering har varit en grundpelare i programmering sedan dess början. I TypeScript, som bygger på JavaScript, blev felhantering mer robust med införandet av async/await i ECMAScript 2017. Innan dess förlitade vi oss ofta på callback-funktioner och löften för att hantera fel i asynkron kod.

Ett alternativ till `try/catch` i TypeScript är att använda felgränser som tillhandahålls av ramverk som React. För server-sidans hantering kan vi använda middleware i plattformar som Express.js för att centralisera felhantering.

När det kommer till implementeringen har TypeScript ingen egen mekanism för felhantering utan förlitar sig på JavaScripts. Anpassade felklasser kan utvidga `Error`-klassen för att erbjuda mer beskrivande felinformation.

## Se även
- [MDN om try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await på MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Använda felgränser i React](https://reactjs.org/docs/error-boundaries.html)
- [Felhantering i Express.js](https://expressjs.com/en/guide/error-handling.html)