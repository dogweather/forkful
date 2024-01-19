---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Confrontare due date significa verificare se una data è anteriore, successiva o uguale a un'altra. Questo è utile per ordinare eventi, stabilire scadenze, o gestire intervalli di tempo.

## Come fare:

Creiamo due oggetti `Date` e utilizziamo l'operatore di confronto `<`, `>`, `==` o `===`.

```JAVASCRIPT
let data1 = new Date('2021-04-20T12:00:00');
let data2 = new Date('2021-04-20T12:05:00');

console.log(data1 > data2); // Risultato: false
console.log(data1 == data2); // Risultato: false - attenzione: confronta l'oggetto, non il valore!
console.log(data1 < data2); // Risultato: true
console.log(data1.getTime() == data2.getTime()); // Risultato: false - corretto modo per confrontare i valori.
```

## Approfondimenti:

Historicamente, l'l'oggetto `Date` in JavaScript esiste sin dalla prima versione. All'inizio, però, era più limitato e meno preciso.

Esiste anche un approccio alternativo per il confronto di date: si può convertire le date in Unix timestamp usando il metodo `.getTime()`, poi confrontare i numeri.

```JAVASCRIPT
let confronto = data1.getTime() - data2.getTime();
if (confronto > 0) {
  console.log("data1 è successiva a data2");
} else if (confronto < 0) {
  console.log("data1 è anteriore a data2");
} else {
  console.log("data1 e data2 sono uguali");
}
```

Il confronto tra date si basa sull'orario UTC (Coordinated Universal Time), che può causare comportamenti inaspettati se non si tiene conto dei fusi orari.

## Vedi Anche:

- [Oggetto Date - MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Metodo getTime() - MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime)
- [Coordinated Universal Time - Wikipedia](https://it.wikipedia.org/wiki/Coordinated_Universal_Time)