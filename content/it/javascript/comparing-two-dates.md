---
title:                "Confrontare due date"
html_title:           "Javascript: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il confronto tra due date è una pratica comune nella programmazione che consente di verificare se una data sia precedente, successiva o uguale a un'altra data. Gli sviluppatori possono utilizzare questa tecnica per gestire le informazioni temporali e per eseguire azioni in base alle date selezionate.

## Come fare:

Per confrontare due date in Javascript, è possibile utilizzare l'operatore di confronto ```>```, ```<```, ```>=```, ```<=``` o l'operatore di uguaglianza ```===```. Questi operatori confrontano i valori numerici delle date e restituiscono un valore booleano (vero o falso).

```Javascript
let data1 = new Date(2021, 4, 10);
let data2 = new Date(2020, 2, 14);

console.log(data1 > data2); // Output: true
console.log(data1 === data2); // Output: false
```

## Approfondimenti:

La gestione delle date è sempre stata una sfida per i programmatori, poiché le date possono essere rappresentate in diversi formati e sono soggette a fusi orari e regole sul cambio dell'ora. 

Un'alternativa all'utilizzo degli operatori di confronto è utilizzare il metodo .getTime() che restituisce il numero di millisecondi trascorsi dal 1 gennaio 1970 ad una data specifica. È possibile quindi confrontare i due numeri ottenuti per determinare se una data sia precedente, successiva o uguale a un'altra.

```Javascript
let data1 = new Date(2021, 4, 10).getTime();
let data2 = new Date(2020, 2, 14).getTime();

console.log(data1 > data2); // Output: true
console.log(data1 === data2); // Output: false
```

Inoltre, esistono innumerevoli librerie Javascript dedicate alla gestione delle date, come Moment.js e Day.js, che offrono metodi per eseguire il confronto tra date in modo più preciso e gestire i diversi formati di data.

## Vedi anche:

- Documentazione ufficiale di Javascript sul confronto tra date: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date/Date.prototype
- Documentazione di Moment.js: https://momentjs.com/
- Documentazione di Day.js: https://day.js.org/