---
title:    "Javascript: Ricerca e sostituzione di testo"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione del testo sono una parte essenziale della programmazione in JavaScript. Questa operazione consente di modificare dinamicamente il contenuto di una variabile o di un elemento HTML all'interno di un programma.

## How To

Per eseguire una ricerca e sostituire del testo in JavaScript, utilizziamo il metodo `replace()` di un oggetto stringa.

Ad esempio, se abbiamo una variabile `nome` che contiene il valore "Giulia", possiamo utilizzare `nome.replace("Giulia", "Maria")` per sostituire la parola "Giulia" con "Maria". Il risultato sarebbe "Maria".

Questo metodo può essere utilizzato anche su un elemento HTML, ad esempio `document.querySelector("h1").replace("Benvenuto", "Ciao")` sostituirà la parola "Benvenuto" con "Ciao" all'interno dell'elemento h1 del documento.

Questo metodo può anche essere utilizzato per sostituire in modo globale tutte le occorrenze di una parola o di una frase, utilizzando il modificatore `/g`.

`nome.replace(/Giulia/g, "Maria")` sostituirà ogni occorrenza di "Giulia" con "Maria" all'interno della variabile `nome`.

## Deep Dive

Il metodo `replace()` in realtà accetta due parametri, uno per la stringa da cercare e uno per la stringa da sostituire. Tuttavia, è anche possibile utilizzare una funzione come secondo parametro, che può essere molto utile in determinati contesti.

Ad esempio, se vogliamo sostituire un numero all'interno di una stringa con quel numero incrementato di uno, possiamo utilizzare una funzione e l'operatore di incremento `++`.

```
let frase = "Il numero è 5";
frase.replace(/\d+/, function(match) {
  return parseInt(match, 10) + 1;
});
```

Questo ci darà come risultato "Il numero è 6".

Un'altra caratteristica utile del metodo `replace()` è la possibilità di utilizzare espressioni regolari per effettuare ricerche più avanzate e sostituire parti specifiche di una stringa.

Per esempio, se vogliamo sostituire tutte le vocali minuscole con il simbolo "-":

```
let frase = "Ciao, come va?";
frase.replace(/[aeiou]/g, "-");
```

Risultato: "C--,- c-- v?".

## Vedi anche

- [Documentazione su replace() di MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Guida alle espressioni regolari in JavaScript](https://flaviocopes.com/javascript-regular-expressions/)