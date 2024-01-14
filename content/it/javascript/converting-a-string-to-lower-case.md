---
title:    "Javascript: Convertire una stringa in minuscolo"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Converting una stringa in minuscolo è un'attività molto comune nella programmazione Javascript, poiché spesso è necessario manipolare le stringhe per ottenere risultati specifici. Conoscere come effettuare questa operazione può aiutare a rendere il codice più efficiente e facile da leggere.

## Come fare

Per convertire una stringa in minuscolo in Javascript, è possibile utilizzare il metodo `toLowerCase()`, che converte tutti i caratteri della stringa in minuscolo.

```Javascript
let stringa = "QUESTA È UNA STRINGA IN MAIUSCOLO";
let nuovaStringa = stringa.toLowerCase();

console.log(nuovaStringa);
```

L'output di questo codice sarà `questa è una stringa in maiuscolo`.

Si noti che il metodo `toLowerCase()` non modifica la stringa originale, ma restituisce una nuova stringa convertita in minuscolo. Per modificare direttamente la stringa originale, è possibile utilizzare l'operatore di assegnazione (`=`).

È inoltre possibile combinare il metodo `toLowerCase()` con altri metodi per effettuare manipolazioni più complesse sulla stringa. Ad esempio, si può sostituire una parte della stringa con un'altra stringa in minuscolo, utilizzando il metodo `replace()`.

```Javascript
let stringa = "Questo è un testo da convertire";
let stringaModificata = stringa.replace("testo", "possiamo fare un esempio");
let nuovaStringa = stringaModificata.toLowerCase();

console.log(nuovaStringa);
```

L'output di questo codice sarà `questo è un esempio da convertire`.

## Approfondimento

La conversione di una stringa in minuscolo è possibile grazie al fatto che in Javascript le stringhe sono immutabili, ovvero non possono essere modificate direttamente. Per effettuare manipolazioni su una stringa, è quindi necessario utilizzare metodi o operatori che restituiscano una nuova stringa modificata.

Inoltre, va tenuto presente che la conversione in minuscolo può variare a seconda della lingua. Ad esempio, nella lingua italiana la lettera "I" con l'accento (Ì) diventa "i" in minuscolo, mentre nella lingua inglese rimane "I". Pertanto, è importante considerare il contesto in cui si utilizza la conversione in minuscolo e capire come essa influenzi il risultato finale.

## Vedi anche

- [Metodo toLowerCase() su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Manipolazione delle stringhe in Javascript](https://www.html.it/articoli/come-manipolare-le-stringhe-in-javascript/)