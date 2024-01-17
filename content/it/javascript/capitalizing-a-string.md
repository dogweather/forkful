---
title:                "Maiuscola di una stringa"
html_title:           "Javascript: Maiuscola di una stringa"
simple_title:         "Maiuscola di una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perche ?
Quando si programma in Javascript, si può capitare di dover manipolare le stringhe, cioè le sequenze di caratteri. Una delle operazioni che si può fare su di esse è quella di "capitalizzare" una stringa, cioè metterla tutta in maiuscolo. Questo può essere utile per uniformare i dati o per rendere più leggibile il testo. I programmatori spesso lo fanno per semplificare il confronto tra due stringhe.

## Come fare:
Per capitalizzare una stringa in Javascript, c'è una semplice funzione nativa chiamata ```toUpperCase()```. Possiamo usare questa funzione su qualsiasi stringa e il risultato sarà la stessa stringa, ma con tutte le lettere maiuscole. Vediamo un esempio:

```javascript
let stringa = "questa è una frase da capitalizzare";
console.log(stringa.toUpperCase());
```

E l'output sarà:

```
QUESTA È UNA FRASE DA CAPITALIZZARE
```

## Approfondimento:
Capitalizzare una stringa è un'operazione molto comune nei linguaggi di programmazione e non solo in Javascript. In passato, questa operazione veniva effettuata con metodi più complessi, ad esempio usando delle funzioni particolari per ogni lettera dell'alfabeto. Fortunatamente, oggi abbiamo a disposizione funzioni native come ```toUpperCase()``` che ci aiutano a velocizzare e semplificare il processo.

Se vogliamo invece capitalizzare solo la prima lettera di una stringa, possiamo usare la funzione ```charAt()```, che restituisce il carattere corrispondente alla posizione indicata all'interno della stringa. In questo modo, possiamo combinare la funzione ```charAt(0)``` con ```toUpperCase()``` per capitalizzare solo il primo carattere. Ad esempio:

```javascript
let stringa = "questa è una frase da capitalizzare";
let capitalizzata = stringa.charAt(0).toUpperCase() + stringa.slice(1);
console.log(capitalizzata);
```

E l'output sarà:

```
Questa è una frase da capitalizzare
```

## Vedi anche:
- [Funzioni stringhe in Javascript](https://www.w3schools.com/js/js_string_methods.asp)
- [Espressioni regolari in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/Regex)
- [Javascript per principianti: le stringhe](https://careersmart.io/articles/680b025b6e14/javascript-per-principianti-le-stringhe)