---
title:                "Gestione degli errori"
date:                  2024-01-26T00:57:50.564283-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"

category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Gestire gli errori significa aspettarsi l'inaspettato; è il modo in cui ci comportiamo quando le cose vanno male nel nostro codice. Lo facciamo per evitare crash e per dare agli utenti un'esperienza fluida, anche quando si verifica l'inaspettato.

## Come fare:
In TypeScript, la gestione degli errori spesso coinvolge blocchi `try`, `catch` e `finally`.

```typescript
function riskyOperation() {
  throw new Error("Qualcosa è andato storto!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Errore catturato:", error.message);
  } finally {
    console.log("Questo viene sempre eseguito, errore o no.");
  }
}

handleErrors();
```

Output di esempio:

```
Errore catturato: Qualcosa è andato storto!
Questo viene sempre eseguito, errore o no.
```

Esempio asincrono con promesse:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Simula un errore
    reject("Fallito miseramente");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Errore asincrono catturato:", error);
  }
}

handleAsyncErrors();
```

Output di esempio:

```
Errore asincrono catturato: Fallito miseramente
```

## Approfondimento
La gestione degli errori è stata una pietra miliare della programmazione fin dalla sua nascita. In TypeScript, che si basa su JavaScript, la gestione degli errori è diventata più robusta con l'introduzione di async/await in ECMAScript 2017. Prima di ciò, ci affidavamo spesso a funzioni di callback e promesse per gestire errori nel codice asincrono.

Un'alternativa a `try/catch` in TypeScript è l'uso di limiti di errore forniti da framework come React. Per la gestione lato server, possiamo utilizzare middleware in piattaforme come Express.js per centralizzare la gestione degli errori.

Dal punto di vista dell'implementazione, TypeScript non ha un proprio meccanismo di gestione degli errori, ma si affida a quello di JavaScript. Le classi di errore personalizzate possono estendere la classe `Error` per offrire informazioni sugli errori più descrittive.

## Vedi anche
- [MDN su try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await su MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Uso dei limiti di errore in React](https://reactjs.org/docs/error-boundaries.html)
- [Gestione degli errori in Express.js](https://expressjs.com/it/guide/error-handling.html)
