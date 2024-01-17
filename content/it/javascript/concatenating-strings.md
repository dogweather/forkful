---
title:                "Unione di stringhe"
html_title:           "Javascript: Unione di stringhe"
simple_title:         "Unione di stringhe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e perché utilizzarlo?
Il concatenamento di stringhe è un'operazione comune nel mondo della programmazione. Consiste nell'unione di due o più stringhe per formare una nuova stringa più lunga. I programmatori eseguono questa operazione per creare output più leggibili e per manipolare e gestire i dati in modi specifici.

## Come: 
```Javascript
console.log('Buongiorno ' + 'a tutti!'); 
// Output: Buongiorno a tutti!
let nome = 'Maria'; 
let saluto = 'Ciao '; 
console.log(saluto + nome); 
// Output: Ciao Maria
```

## Approfondimento: 
Il concatenamento di stringhe è una tecnica ampiamente utilizzata fin dai primi giorni della programmazione. In passato, i programmatori utilizzavano il metodo "concat" per unire le stringhe, ma con l'avvento di ES6, è possibile utilizzare il simbolo "+" per concatenare le stringhe. Esistono anche alternative come il template literals e il metodo "join". Quando si concatenano stringhe, è importante tenere conto del tipo di dati utilizzati per evitare errori e problemi di codifica.

## Vedi anche: 
- [Documentazione di Javascript sull'operatore di concatenamento](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
- [Tutorial su come concatenare stringhe in Javascript](https://www.w3schools.com/js/tryit.asp?filename=tryjs_strings_concat)
- [Esposizione più approfondita sull'utilizzo di template literals per il concatenamento di stringhe](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)