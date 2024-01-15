---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Javascript: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Calcolare una data nel futuro o nel passato è utile quando si deve pianificare una programmazione o tenere traccia del tempo trascorso. Ad esempio, è possibile utilizzarlo per creare promemoria o visualizzare il numero di giorni mancanti prima di una data importante.

## Come fare
In Javascript, esistono diverse librerie e funzioni incorporate che consentono di calcolare una data nel futuro o nel passato. Una delle opzioni più semplici è utilizzare il metodo `getDate()` dell'oggetto `Date`, specificando il numero di giorni da aggiungere o sottrarre. Ad esempio, per ottenere la data di domani, si può utilizzare questo codice:

```Javascript
let oggi = new Date(); // ottiene la data di oggi
let domani = oggi.getDate() + 1; // aggiunge un giorno alla data di oggi
console.log(domani); // stampa la data di domani in formato numerico
```

Il codice dovrebbe stampare "1", poiché oggi è il primo giorno del mese. Per visualizzare la data completa in formato leggibile, è possibile utilizzare il metodo `toLocaleDateString()`:

```Javascript
console.log(domani.toLocaleDateString()); // stampa la data di domani nel formato locale
```

L'output dovrebbe essere qualcosa del genere: "giovedì, 2 settembre 2021". In questo modo, si può facilmente personalizzare il formato della data in base alle proprie preferenze.

## Deep Dive
Se si desidera calcolare una data nel futuro o nel passato in base ad un'unità di tempo diversa dai giorni, come settimane, mesi o anni, si può utilizzare il metodo `set...()` dell'oggetto `Date`. Ad esempio, per ottenere la data che si trova tra tre mesi dalla data di oggi, si può utilizzare questo codice:

```Javascript
let oggi = new Date(); // ottiene la data di oggi
let traTreMesi = oggi.setMonth(oggi.getMonth() + 3); // aggiunge tre mesi alla data di oggi
console.log(traTreMesi.toLocaleDateString()); // stampa la data tra tre mesi in formato leggibile
```

Tenendo conto che oggi è il 1° settembre 2021, l'output sarà "venerdì, 1 dicembre 2021". Ovviamente, si può utilizzare questo concetto per calcolare una data nel passato semplicemente sottraendo i mesi desiderati. D'altra parte, se si utilizza una libreria come Moment.js, è possibile effettuare operazioni ancora più complesse, come calcolare una data in base ad un preciso formato o utilizzando una data di riferimento differente da quella di oggi.

## Vedi anche
- [Documentazione ufficiale di Javascript su oggetto Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Calcolo di date nel futuro e nel passato in Python](https://www.pythonpool.com/python-datetime-add-days/)