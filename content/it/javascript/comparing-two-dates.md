---
title:                "Javascript: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare le date è un'attività fondamentale nella programmazione, in particolare quando si lavora con applicazioni che gestiscono eventi e scadenze. Confrontare due date può aiutare a determinare quali eventi sono passati, futuri o uguali.

## Come fare

Per comparare due date in Javascript, è necessario utilizzare l'oggetto `Date`. Ad esempio, se si vuole confrontare due date di nascita per determinare chi è più grande, si può utilizzare il seguente codice:

```Javascript
let date1 = new Date("1990-03-15");
let date2 = new Date("1995-12-09");

if (date1 > date2) {
  console.log("La prima persona è più grande della seconda.");
} else if (date2 > date1) {
  console.log("La seconda persona è più grande della prima.");
} else {
  console.log("Le due persone hanno la stessa età.");
}
```

L'output di questo codice sarà: "La seconda persona è più grande della prima." Poiché la seconda persona è nata successivamente alla prima.

Un altro modo per comparare date è utilizzare il metodo `getTime()`, che restituisce il numero di millisecondi trascorsi dall'1 gennaio 1970 alla data specificata.

```Javascript
if (date1.getTime() > date2.getTime()) {
  console.log("La prima persona è più grande della seconda.");
} else if (date2.getTime() > date1.getTime()) {
  console.log("La seconda persona è più grande della prima.");
} else {
  console.log("Le due persone hanno la stessa età.");
}
```

Inoltre, è possibile utilizzare gli operatori `==` e `>=` per determinare se due date sono uguali o se una è successiva all'altra.

## Approfondimento

Quando si confrontano date in Javascript, è importante ricordare che ogni data è creata come istanza dell'oggetto `Date`, che rappresenta un timestamp preciso, ovvero la data e l'ora specifiche in cui è stata creata.

È importante anche prestare attenzione alle diverses formati di input delle date. Ad esempio, la data "01/02/2020" potrebbe essere interpretata come 1 febbraio 2020 o come 2 gennaio 2020, a seconda del formato specificato.

Inoltre, gli oggetti `Date` non hanno solo il metodo `getTime()` per confrontare le date, ma possono essere utilizzati anche con altri metodi utili come `getMonth()` e `getFullYear()` per ottenere informazioni specifiche sulla data.

## Vedi anche

- [Documentazione di Javascript sull'oggetto Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutorial su come comparare date in Javascript](https://www.w3schools.com/js/js_date_methods.asp)
- [Articolo su come usare l'oggetto Date in Javascript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)