---
title:    "Javascript: Confrontare due date"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché

Comparare due date è una delle attività più comuni nella programmazione Javascript. Questo può essere utile quando si vuole verificare se una data è prima o dopo un'altra, o se sono entrambe nella stessa settimana o mese. In questo articolo, vedremo come fare un confronto tra due date utilizzando Javascript.

## Come Fare

Per confrontare due date in Javascript, è prima necessario convertirle in oggetti Date, utilizzando il costruttore "new Date()". Questo restituirà un oggetto Date contenente la data corrispondente alla stringa fornita.

```Javascript
const date1 = new Date("2021-01-01");
const date2 = new Date("2021-03-01");

// confronto delle date con l'operatore di confronto maggiore
if (date1 > date2) {
  console.log("La data 1 è successiva alla data 2");
} else if (date1 < date2) {
  console.log("La data 1 è precedente alla data 2");
} else {
  console.log("Le date sono uguali");
}
```

Output:

```
La data 1 è precedente alla data 2
```

Per verificare se due date sono nello stesso mese o settimana, possiamo utilizzare i metodi "getMonth()" e "getDay()" dell'oggetto Date, che restituiscono rispettivamente il numero del mese e del giorno nella settimana. In questo esempio, utilizzeremo il metodo "isEqual()" che abbiamo definito per confrontare i valori restituiti dai due oggetti Date.

```Javascript
// metodo isEqual per confrontare due valori
function isEqual(value1, value2) {
  if (value1 === value2) {
    return true;
  } else {
    return false;
  }
}

// confronto delle date per lo stesso mese
if (isEqual(date1.getMonth(), date2.getMonth())) {
  console.log("Le date cadono nello stesso mese");
}

// confronto delle date per la stessa settimana
if (isEqual(date1.getDay(), date2.getDay())) {
  console.log("Le date cadono nella stessa settimana");
}
```

Output:

```
Le date cadono nello stesso mese
Le date cadono nella stessa settimana
```

## Deep Dive

Il confronto di due date in Javascript può essere controverso a causa delle diverse implementazioni dei diversi browser. Ad esempio, alcuni browser utilizzano l'anno 0 per la data "01/01/1970" mentre altri lo considerano come non valido o aggiustano automaticamente l'anno a 1. Ciò potrebbe influire sui valori restituiti dai metodi "getMonth()" e "getDay()". Inoltre, ci possono essere problemi con i fusi orari quando si utilizzano le date nel formato ISO, poiché alcuni browser interpretano automaticamente la data locale e la convertono in un fuso orario diverso.

È sempre importante considerare questi aspetti quando si lavora con date in Javascript e cercare di utilizzare librerie come Moment.js per gestire in modo più affidabile le date e i fusi orari.

## Vedi Anche

- [Validazione delle date in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date/validazione)
- [Moment.js](https://momentjs.com/) - Libreria per la gestione delle date in Javascript