---
title:                "Trova la lunghezza di una stringa."
html_title:           "Javascript: Trova la lunghezza di una stringa."
simple_title:         "Trova la lunghezza di una stringa."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perché?

La lunghezza di una stringa si riferisce alla quantità di caratteri presenti all'interno di una stringa di testo in Javascript. I programmatori spesso hanno bisogno di trovare la lunghezza di una stringa per vari motivi, come ad esempio per la gestione dei dati inseriti dagli utenti o per manipolare il testo all'interno di un'applicazione.

## Come fare:

Per trovare la lunghezza di una stringa, è possibile utilizzare il metodo ```length``` in Javascript. Di seguito è riportato un esempio di codice con un'input di esempio e l'output atteso:

```Javascript
let stringa = "Ciao mondo";
console.log(stringa.length);

// Output: 10
```

In questo esempio, la variabile ```stringa``` contiene una stringa di testo con 10 caratteri (spazi inclusi). Utilizzando il metodo ```length```, possiamo trovare la lunghezza della stringa e stamparla a console.

## Approfondimento:

La necessità di trovare la lunghezza di una stringa risale ai primi giorni della programmazione, quando il linguaggio di programmazione C è stato sviluppato negli anni '70. In C, la lunghezza di una stringa è stata trovata contando i byte necessari per rappresentare il testo in memoria.

In Javascript, esistono anche altri modi per trovare la lunghezza di una stringa, come ad esempio utilizzando il metodo ```split``` per suddividere la stringa in un array e contare il numero di elementi. Tuttavia, l'utilizzo del metodo ```length``` è il modo più efficiente per ottenere questo risultato in Javascript.

## Vedi anche:

- [Documentazione di Javascript sul metodo ```length```] (https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Articolo su come trovare la lunghezza di una stringa in altri linguaggi di programmazione] (https://www.mkyong.com/javascript/how-to-get-the-length-of-a-string-in-javascript/)
- [Un approfondimento sulle stringhe in Javascript] (https://www.freecodecamp.org/news/javascript-string-manipulation/)