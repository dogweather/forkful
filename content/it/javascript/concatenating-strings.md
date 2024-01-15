---
title:                "Concatenazione di stringhe"
html_title:           "Javascript: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione comune nella programmazione JavaScript perché consente di unire due o più stringhe per creare una nuova stringa. Questo è utile quando si desidera combinare dati diversi o creare un output complesso.

## Come Fare

Per concatenare stringhe in JavaScript, è possibile utilizzare l'operatore "+" o il metodo "concat()". Ecco degli esempi di entrambi i metodi:

```Javascript
// Utilizzo dell'operatore "+"
let stringa1 = "Ciao";
let stringa2 = "Mondo";
let risultato = stringa1 + " " + stringa2;
console.log(risultato); // Output: Ciao Mondo

// Utilizzo del metodo "concat()"
let stringa3 = "Buongiorno";
let stringa4 = "a tutti";
let risultato2 = stringa3.concat(" ", stringa4);
console.log(risultato2); // Output: Buongiorno a tutti
```

In questo esempio, stiamo concatenando le stringhe "stringa1" e "stringa2" utilizzando l'operatore "+". Nota come le stringhe siano separate da uno spazio per ottenere una stringa unica con gli spazi corretti.

Nel secondo esempio, utilizziamo il metodo "concat()" per unire le stringhe "stringa3" e "stringa4". Il metodo supporta più argomenti, quindi è possibile concatenare più di due stringhe in una volta.

## Approfondimento

Ci sono alcuni dettagli importanti da conoscere quando si concatenano stringhe in JavaScript. Ad esempio, il tipo di dato degli operandi e dell'output può influire sul risultato.

Se uno dei due operandi è un numero, entrambi gli operandi verranno convertiti in stringhe prima di essere concatenati. Ad esempio:

```Javascript
let numero = 123;
let stringa = "456";
let risultato = numero + stringa;
console.log(risultato); // Output: 123456
```

In questo caso, il numero 123 viene convertito in una stringa e concatenato alla stringa "456", producendo l'output "123456".

Inoltre, se si cerca di concatenare una stringa vuota, questa verrà semplicemente ignorata e l'output sarà la stringa non vuota. Ad esempio:

```Javascript
let stringa1 = "Ciao";
let stringa2 = "";
let risultato = stringa1 + stringa2;
console.log(risultato); // Output: Ciao
```

La stringa vuota tra "Ciao" e l'operatore "+" viene semplicemente ignorata e quindi l'output rimane l'unica stringa non vuota.

## Vedi Anche

Per maggiori informazioni su come utilizzare le stringhe in JavaScript, consulta questi articoli:

- [Working with Strings in JavaScript](https://www.w3schools.com/js/js_strings.asp)
- [String Concatenation in JavaScript](https://www.geeksforgeeks.org/javascript-string-concatenation/)
- [The "+" Operator in JavaScript](https://www.w3schools.com/jsref/jsref_operators.asp)