---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Trovare la lunghezza di una stringa significa contare il numero di caratteri in essa. I programmatori usano spesso questa funzione nelle applicazioni web, per esempio per convalidare l'input dell'utente o per manipolare le stringhe.

## Come Fare
Ecco come ottenere la lunghezza di una stringa con JavaScript.

```javascript
var str = "Ciao, mondo!";
var n = str.length;

console.log(n); 
```

Questo restituirà: 

```javascript
13
```
## Approfondimenti
La proprietà length esiste da quando JavaScript è stato creato, circa 25 anni fa. Non ci sono reali alternative a questa proprietà in JavaScript, dato che è la più veloce e diretta per ottenere il conteggio dei caratteri. Vale la pena notare che length ritorna il numero di unità di codice UTF-16, che non sempre corrispondono al numero di caratteri.

## Da Vedere Anche
Per un'ulteriore lettura, tutto ciò che riguarda le stringhe in JavaScript, controlla i seguenti link:

- Documentazione MDN su Stringhe e Caratteri: https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/Text_formatting
- Dettagli su UTF-16 su Wikipedia: https://it.wikipedia.org/wiki/UTF-16
- Guide JavaScript su W3Schools: https://www.w3schools.com/js/js_string_methods.asp