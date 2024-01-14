---
title:                "Javascript: Confrontare due date."
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è un'operazione comune nella programmazione, soprattutto quando si lavora con date e orari. Questo è importante per garantire che le operazioni eseguite sulle date siano corrette e che i dati siano gestiti in modo accurato. Imparare a confrontare due date è fondamentale per scrivere codice robusto e senza errori.

## Come fare

Per confrontare due date in Javascript, è necessario utilizzare l'oggetto `Date()` e i suoi metodi. Ad esempio, se vogliamo confrontare due date per vedere se sono uguali, possiamo utilizzare il metodo `.getTime()` che restituisce il numero di millisecondi trascorsi dalla mezzanotte del 1º gennaio 1970. Il codice sarebbe simile a questo:

```
let date1 = new Date("2021-05-10");
let date2 = new Date("2021-05-10");

if(date1.getTime() === date2.getTime()) {
    console.log("Le date sono uguali!");
}
```

In questo esempio, entrambe le date sono uguali e quindi il messaggio "Le date sono uguali!" verrà visualizzato sulla console.

È possibile anche confrontare le date per vedere quale è più recente. Utilizzando il metodo `.getTime()` come prima, possiamo confrontare i millisecondi delle due date e determinare quale sia più grande. Ecco un esempio:

```
let date1 = new Date("2021-05-10");
let date2 = new Date("2021-05-20");

if(date1.getTime() < date2.getTime()) {
    console.log("La prima data è antecedente alla seconda data!");
}
```

In questo caso, il messaggio "La prima data è antecedente alla seconda data!" verrà visualizzato sulla console, poiché la data1 è antecedente rispetto alla data2.

## Approfondimento

Nel confrontare due date, è importante considerare anche i fusi orari. Potrebbe essere necessario convertire le date in millisecondi in base al fuso orario in cui si trovano prima di confrontarle. Inoltre, ci sono molti altri metodi dell'oggetto `Date()` che possono essere utilizzati per manipolare e confrontare le date.

Un altro aspetto importante è la gestione dei dati di tipo diverso. Ad esempio, se una delle date è inserita come stringa, è necessario convertirla in un oggetto `Date()` prima di confrontarla con un'altra data.

## Vedi anche

- [Documentazione di Javascript sull'oggetto Date()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutorial di W3Schools sui metodi dell'oggetto Date()](https://www.w3schools.com/jsref/jsref_obj_date.asp)