---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) serve a separare i log degli errori dall'output standard (stdout). I programmatori lo fanno per organizzare meglio i logs e gestire gli errori in modo più efficace.

## How to:
Per scrivere su stderr in JavaScript, usa `console.error()` o `process.stderr.write()`:

```Javascript
// Usando console.error()
console.error('Questo è un errore!');

// Usando process.stderr.write() nel contesto di Node.js
process.stderr.write('Questo è un errore!\n');
```

Output:
```
Questo è un errore!
```

## Deep Dive
In passato, stdout e stderr sono stati introdotti per consentire ai programmi in ambiente UNIX di distinguere l'output normal da messaggi di errore. In JavaScript, `console.error()` è spesso usato per debug o per registrare messaggi di errore, mentre `process.stderr.write()` è specifico per Node.js e permette di scrivere buffer o stringhe su stderr, senza aggiungere una nuova linea automaticamente, al contrario di `console.error()`.

## See Also
- Documentazione MDN su `console.error()`: [MDN console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- Documentazione Node.js su `process.stderr`: [Node.js process.stderr](https://nodejs.org/api/process.html#process_process_stderr)
