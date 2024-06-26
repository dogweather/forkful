---
date: 2024-01-26 00:58:40.021875-07:00
description: "Hur man g\xF6r: I TypeScript involverar hantering av fel ofta `try`,\
  \ `catch` och `finally` block."
lastmod: '2024-03-13T22:44:37.663413-06:00'
model: gpt-4-1106-preview
summary: I TypeScript involverar hantering av fel ofta `try`, `catch` och `finally`
  block.
title: Hantering av fel
weight: 16
---

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
