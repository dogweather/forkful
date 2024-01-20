---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Estrazione Di Sottostringhe in TypeScript: Il Tuo Nuovo Superpotere

## Che Cosa e Perché?

L'estrazione di sottostringhe è l'arte di prelevare parti specifiche di una stringa. Noi programmatori lo faciamo per manipolare e analizzare i dati in modi che non sarebbero possibili con stringhe intere.

## Come Fare:

Utilizziamo i metodi `slice()`, `substr()` e `substring()` in TypeScript per ottenere sottostringhe. Ecco come:

``` TypeScript 
let str = 'Guardiamo i poteri del TypeScript!';

console.log(str.slice(10, 17));  // Output: 'i poteri'
console.log(str.substr(10, 7));   // Output: 'i poteri'
console.log(str.substring(10, 17)); // Output: 'i poteri'
```
Tutti questi tre metodi restituiscono la sottostringa 'i poteri'.

`slice()` e `substring()` accettano due indici per definire l'inizio e la fine della sottostringa. `substr()` invece, accetta l'indice di inizio e la lunghezza della sottostringa.

## Approfondimenti:

### Contesto storico:
L'abilità di estrarre sottostringhe esiste da quando le stringhe sono state introdotte nella programmazione. Nonostante ciò, l'implementazione varia da un linguaggio all'altro.

### Alternative:
In TypeScript, `slice()` e `substring()` si comportano in modo simile ma hanno delle differenze sottili. Se fornisci indici negativi, `slice()` li interpreterà come riferimenti dal fondo della stringa, mentre `substring()` li tratterà come zero.

`substr()`, a differenza degli altri due, è considerato obsoleto e potrebbe non essere supportato in tutte le piattaforme JavaScript.

### Dettagli di Implementazione:
La scelta del metodo da utilizzare dipende dal tuo caso specifico. Ricorda, `substring()` e `slice()` accettano indici di inizio e fine, mentre `substr()` accetta indice di inizio e lunghezza.

## Link Utili:

- MDN Web Docs: [Basic Data Structures in JavaScript: Strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Data_structures#strings)
- W3Schools: [JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)