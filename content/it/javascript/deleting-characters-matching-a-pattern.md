---
title:                "Eliminazione di caratteri corrispondenti a un pattern"
html_title:           "Javascript: Eliminazione di caratteri corrispondenti a un pattern"
simple_title:         "Eliminazione di caratteri corrispondenti a un pattern"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il processo di eliminazione di caratteri che corrispondono ad un certo pattern è una pratica comune tra i programmatori. Questa operazione consiste nel cercare nel testo una determinata combinazione di caratteri e rimuoverla dalla stringa originale. I programmatori fanno ciò per rendere i loro codici più efficienti, puliti e facili da leggere.

## Come fare:
Per eliminare caratteri che corrispondono ad un pattern in JavaScript, puoi utilizzare il metodo .replace(). Questo metodo accetta due argomenti: il primo è il pattern da cercare e il secondo è la stringa vuota "" che indica l'eliminazione del testo corrispondente. Di seguito un esempio di codice:

```Javascript
let stringa = "Questo testo ha delle lettere maiuscole";
let nuovoTesto = stringa.replace(/[A-Z]/g, "");
console.log(nuovoTesto);
// Output: questo testo ha delle lettere minuscole
```

In questo esempio, il metodo .replace() cerca tutte le lettere maiuscole nella stringa "stringa" e le sostituisce con una stringa vuota, quindi elimina tutte le lettere maiuscole.

## Approfondimenti:
La pratica di eliminare caratteri che corrispondono ad un certo pattern è stata originariamente introdotta nell'epoca dei primi editor di testo, quando i programmatori dovevano modificare manualmente i file di testo. Oggi, ci sono anche altri metodi per ottenere lo stesso risultato, come l'utilizzo delle espressioni regolari o delle funzioni di array in JavaScript.

## Vedi anche:
- Documentazione ufficiale di .replace() in JavaScript: [https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Tutorial sulle espressioni regolari in JavaScript: [https://www.w3schools.com/js/js_regexp.asp](https://www.w3schools.com/js/js_regexp.asp)