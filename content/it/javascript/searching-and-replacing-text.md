---
title:                "Ricerca e sostituzione di testo"
html_title:           "Javascript: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Ricercare e sostituire il testo è un'azione comune nel mondo della programmazione. Consiste nel cercare una specifica parola o frase all'interno di un testo e sostituirla con un'altra. Questo è spesso utilizzato per correggere errori o per effettuare modifiche su larga scala in un programma. 

## Come fare:
Ecco una semplice funzione Javascript per cercare e sostituire il testo all'interno di una stringa:
```Javascript
function replaceText(str, old, new) {
  return str.replace(old, new);
}
console.log(replaceText("Questo è un esempio di testo", "esempio", "prova"));
```
Output: "Questo è un prova di testo"

## Approfondimento:
La ricerca e la sostituzione di testo sono state introdotte per la prima volta nei linguaggi di programmazione negli anni '50. Oggi è possibile utilizzare strumenti di ricerca e sostituzione avanzati all'interno di editor di testo o IDE come Visual Studio Code. In alternativa, è possibile utilizzare espressioni regolari per effettuare ricerche e sostituzioni più complesse.

## Vedi anche:
- [Documentazione sulla funzione replace di Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Utilizzo delle espressioni regolari in Javascript](https://www.regular-expressions.info/javascript.html)