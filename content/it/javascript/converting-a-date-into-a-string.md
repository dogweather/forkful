---
title:    "Javascript: Convertire una data in una stringa"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Spesso, quando si lavora con date in Javascript, è necessario convertirle in formato stringa per renderle più leggibili per l'utente o per essere utilizzate in altre parti del codice. Vediamo quindi come eseguire questa conversione in modo semplice ed efficace.

## Come
Per convertire una data in stringa in Javascript, possiamo utilizzare il metodo `toString()` sull'oggetto `Date`. Esempi di codice e output:

```Javascript
var data = new Date();
console.log(data.toString());
// Output: Wed Aug 18 2021 16:31:27 GMT+0200 (Central European Summer Time)

var data2 = new Date('2021-08-15');
console.log(data2.toString());
// Output: Sun Aug 15 2021 00:00:00 GMT+0200 (Central European Summer Time)
```

Come si può notare, il metodo `toString()` restituisce una stringa che indica la data, l'ora e la fuso orario corrispondente.

## Deep Dive
Quando viene eseguita la conversione in stringa di una data, Javascript utilizza il formato di stringa standard ISO 8601, che è "yyyy-MM-ddTHH:mm:ss.sssZ". Nel nostro esempio, l'output corrisponde a questo formato ma con l'aggiunta degli elementi corrispondenti all'ora e al fuso orario. Inoltre, è importante notare che il metodo `toString()` converte sempre la data in base al fuso orario impostato nel sistema in cui il codice viene eseguito.

Per personalizzare il formato della stringa, possiamo utilizzare il metodo `toLocaleString()` specificando il fuso orario e le opzioni desiderate. Esempio di codice e output:

```Javascript
var data = new Date('2021-08-15 08:30:00');
console.log(data.toLocaleString('it-IT', { timeZone: 'Europe/Rome' }));
// Output: 15/08/2021, 08:30:00
```

In questo caso, abbiamo specificato un formato personalizzato per le impostazioni locali italiane, ottenendo una stringa leggibile e comprensibile per gli utenti italiani.

## Vedi anche
- [Documentazione ufficiale di Javascript su Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Informazioni sul formato di stringa ISO 8601](https://www.iso.org/glossary-for-iso-8601-date-and-time-format.html)