---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:43.024366-07:00
description: "Come fare: In JavaScript, non esiste un metodo incorporato per capitalizzare\
  \ direttamente le stringhe, ma \xE8 semplice da implementare utilizzando i metodi\u2026"
lastmod: '2024-03-13T22:44:43.795407-06:00'
model: gpt-4-0125-preview
summary: "In JavaScript, non esiste un metodo incorporato per capitalizzare direttamente\
  \ le stringhe, ma \xE8 semplice da implementare utilizzando i metodi base di manipolazione\
  \ delle stringhe."
title: Capitalizzare una stringa
weight: 2
---

## Come fare:
In JavaScript, non esiste un metodo incorporato per capitalizzare direttamente le stringhe, ma è semplice da implementare utilizzando i metodi base di manipolazione delle stringhe.

### Usando JavaScript Standard
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Output: "Hello world"
```

### Versione ES6
Con i template literals di ES6, la funzione può essere scritta in un modo più succinto:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Output: "Hello ES6"
```

### Usando Lodash
Lodash è una popolare libreria di utilità di terze parti che offre una vasta gamma di funzioni per manipolare e lavorare con i valori JavaScript, inclusi le stringhe. Per capitalizzare una stringa usando Lodash:
```javascript
// Prima, installa lodash se non l’hai già fatto: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // Output: "Lodash example"
```
_Nota come Lodash non solo capitalizza la prima lettera ma converte anche il resto della stringa in minuscolo, differendo leggermente dall'implementazione JavaScript semplice._

### Usando CSS (Solo a Scopo di Visualizzazione)
Se l'obiettivo è capitalizzare il testo per la visualizzazione nell'UI, può essere utilizzato il CSS:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Viene visualizzato come "Hello css" -->
```
**Nota:** Questo metodo cambia il modo in cui il testo appare sulla pagina web senza modificare la stringa stessa in JavaScript.
