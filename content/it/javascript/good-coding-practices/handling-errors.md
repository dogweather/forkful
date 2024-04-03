---
date: 2024-01-26 00:54:10.645836-07:00
description: "La gestione degli errori \xE8 il modo in cui si gestiscono le situazioni\
  \ impreviste nel codice. \xC8 fondamentale perch\xE9 permette ai tuoi programmi\
  \ di fallire\u2026"
lastmod: '2024-03-13T22:44:43.819926-06:00'
model: gpt-4-1106-preview
summary: "La gestione degli errori \xE8 il modo in cui si gestiscono le situazioni\
  \ impreviste nel codice."
title: Gestione degli errori
weight: 16
---

## Cosa & Perché?

La gestione degli errori è il modo in cui si gestiscono le situazioni impreviste nel codice. È fondamentale perché permette ai tuoi programmi di fallire con eleganza e fornisce istruzioni chiare agli utenti, invece di semplicemente bloccarsi o "crashare".

## Come fare:

Ecco il classico blocco `try-catch`:

```javascript
try {
  // Codice che potrebbe generare un errore
  let result = potentiallyRiskyOperation();
  console.log('Successo:', result);
} catch (error) {
  // Cosa fare se viene generato un errore
  console.error('Ops:', error.message);
}
```

Esempio di output quando non si verifica alcun errore:
```
Successo: 42
```

E quando si verifica un errore:
```
Ops: Qualcosa è andato storto
```

Per il codice asincrono, dove sono coinvolti i promise, utilizza `try-catch` in una funzione `async`:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Dati recuperati:', data);
  } catch (error) {
    console.error('Errore nel recupero dati:', error.message);
  }
}

fetchData();
```

## Approfondimento

La gestione degli errori in JavaScript è evoluta. Ai vecchi tempi (ES3, circa 1999), avevamo solo il blocco `try-catch`. Non era super flessibile, ma faceva il suo lavoro.

ES6 (2015) ha introdotto i Promise e ci ha dato `.then()` e `.catch()`, permettendoci di gestire gli errori asincroni in modo più elegante.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Dati recuperati:', data))
  .catch(error => console.error('Errore nel recupero dati:', error.message));
```

Per quanto riguarda i dettagli di implementazione, quando viene lanciato un errore, i motori JavaScript creano un oggetto `Error` con proprietà utili come `message` e `stack`. È inoltre possibile creare tipi di errori personalizzati estendendo la classe `Error` – comodo per applicazioni più complesse.

Alternative? Potresti ignorare la gestione degli errori (pessima idea), utilizzare callback con parametri che prevedono l'errore per primo (ciao, stile Node.js), o sperimentare con librerie e framework che offrono le loro soluzioni.

## Vedi anche

Per saperne di più sulla gestione degli errori:

- MDN su try-catch: [MDN try...catch](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN funzione async](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Statements/async_function)
- Una guida ai Promise: [MDN Promises](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Creazione e lancio di errori personalizzati: [MDN Error](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Error)
