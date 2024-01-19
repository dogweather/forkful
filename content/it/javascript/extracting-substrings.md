---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Oltre alle Stringhe in JavaScript: L'estrazione delle Sottostringhe

## Cos'è e Perché?
L'estrazione di sottostringhe si riferisce al ritirare spezzi specifici da una stringa di testo. I programmatori la usano per manipolare, analizzare e ristrutturare dati testuali.

## Come fare:
Ecco come estraiamo le sottostringhe in JavaScript:

```Javascript
let stringa = 'Ciao, mi chiamo Mario';
let sottostringa = stringa.substring(0, 4);

console.log(sottostringa); // Output: 'Ciao'
```

Simple, vero? La funzione `substring()` prende due argomenti: il punto di inizio e il punto di fine dell'estrazione.

```Javascript 
let stringa = 'Ciao, mi chiamo Mario';
let sottostringa = stringa.substr(5, 18); 

console.log(sottostringa); // Output: ', mi chiamo Mario'
```
In `substr()`, il primo argomento è l'indice di inizio, e il secondo è la lunghezza della sottostringa che vuoi.

## Approfondimenti

1) Storicamente, queste funzioni esistono da quando JavaScript è stato creato, contribuendo alla manipolazione delle stringhe dal 1995. 

2) Oltre a `substring()` e `substr()`, abbiamo anche `slice()`, che funziona in modo simile ma può accettare valori negativi per estrarre dal fondo.

```Javascript
let stringa = 'Ciao, mi chiamo Mario';
let sottostringa = stringa.slice(-5); 

console.log(sottostringa); // Output: 'Mario'
```

3) Si noti che queste funzioni non modificano la stringa originale, ma ritornano una nuova sottostringa.

## Vedi Anche
Per ulteriori informazioni e approfondimenti sulla manipolazione delle stringhe in JavaScript, visita:

1) [MDN: String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
2) [MDN: String.prototype.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
3) [MDN: String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)

Ricorda, la pratica porta alla perfezione. Buona programmazione!