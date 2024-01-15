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

## Perché

Ci sono diverse situazioni in cui potresti voler confrontare due date in un programma Javascript. Ad esempio, potresti avere bisogno di verificare se una data è successiva o precedente rispetto ad un'altra per gestire una prenotazione o un evento.

## Come fare

Per confrontare due date in Javascript, dobbiamo convertirle in un formato che possiamo confrontare facilmente, come ad esempio il formato numerico dei millisecondi. Utilizzando il metodo `getTime()` possiamo ottenere i millisecondi da una data e quindi confrontare i risultati.

```Javascript
const date1 = new Date("2021-01-01");
const date2 = new Date("2021-01-15");

const milliseconds1 = date1.getTime();
const milliseconds2 = date2.getTime();

if (milliseconds1 < milliseconds2) {
  console.log("La prima data è antecedente alla seconda data.");
} else if (milliseconds1 > milliseconds2) {
  console.log("La prima data è successiva alla seconda data.");
} else {
  console.log("Le due date sono uguali.");
}
```

Questa semplice operazione ci permette di confrontare facilmente due date e gestire le logiche del nostro programma in base al risultato.

## Approfondimento

Oltre al metodo `getTime()`, esistono anche altri metodi che possiamo utilizzare per confrontare due date in Javascript. Ad esempio, il metodo `getTimezoneOffset()` ci permette di ottenere la differenza in minuti tra il fuso orario locale e il fuso orario UTC. Possiamo utilizzare questa informazione per gestire il confronto di date in fusi orari diversi.

Un altro metodo utile è `getDay()`, che restituisce un numero da 0 a 6 che rappresenta il giorno della settimana della data specificata. Questo può essere utile per confrontare date in base al giorno della settimana, ad esempio per gestire promozioni o eventi ricorrenti.

## Vedi anche

Se vuoi approfondire l'argomento, puoi consultare questi siti:

- [Documentazione ufficiale di Javascript su Date Object](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Approfondimenti sui metodi per confrontare date in Javascript](https://www.w3schools.com/js/js_date_methods.asp)
- [Esempi pratici su come utilizzare il confronto di date in programmi Javascript](https://www.digitalocean.com/community/tutorials/how-to-compare-dates-in-javascript)