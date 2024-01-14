---
title:                "Javascript: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
Il processo di ricerca e sostituzione del testo è fondamentale nella programmazione Javascript per poter automatizzare e semplificare la gestione del testo. Ad esempio, può essere utilizzato per correggere eventuali errori di battitura in modo rapido ed efficiente.

## Come Fare
Per eseguire una ricerca e sostituzione del testo in Javascript, è necessario utilizzare il metodo `replace()`. Vediamo un esempio pratico:

```Javascript
var str = "Ciao, sono un programmatore!";
var newStr = str.replace("programmatore", "sviluppatore");

console.log(newStr);
```
Output: Ciao, sono un sviluppatore!

In questo esempio, è stato cercato il termine "programmatore" e sostituito con "sviluppatore" all'interno della stringa "Ciao, sono un programmatore!". Il risultato finale è stato quindi "Ciao, sono un sviluppatore!".

Inoltre, è possibile utilizzare le espressioni regolari per effettuare una sostituzione di più occorrenze di un determinato pattern all'interno di una stringa. Vediamo un altro esempio:

```Javascript
var str = "Javascript è divertente ma può essere complicato a volte.";
var newStr = str.replace(/complicato/g, "difficile");

console.log(newStr);
```
Output: Javascript è divertente ma può essere difficile a volte.

In questo caso, la parola "complicato" è stata sostituita con "difficile" all'interno della stringa.

## Approfondimento
Il metodo `replace()` non altera la stringa originale, ma restituisce una nuova stringa con le modifiche effettuate. Inoltre, è possibile combinare l'utilizzo delle espressioni regolari con funzioni per manipolare la stringa durante il processo di sostituzione.

Ad esempio, è possibile utilizzare la funzione `toUpperCase()` per sostituire una parola con la sua versione maiuscola:

```Javascript
var str = "cercando e sostituendo";
var newStr = str.replace(/e/g, function(match) {
  return match.toUpperCase();
});

console.log(newStr);
```
Output: cErcando E sostituendo

In questo esempio, la lettera "e" è stata sostituita con "E" usando la funzione `toUpperCase()`.

## Vedi Anche
- [Documentazione del metodo replace() in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Tutorial sulle espressioni regolari in Javascript](https://www.html.it/pag/27667/espressioni-regolari-in-javascript/)