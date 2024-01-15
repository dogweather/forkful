---
title:                "Ottenere la data corrente."
html_title:           "Javascript: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Sei stanco di scrivere la data corrente manualmente ogni volta che sviluppi un nuovo progetto? Con Javascript, puoi facilmente ottenere la data corrente all'interno del tuo codice, risparmiando tempo e sforzi.

## Come

Per ottenere la data corrente in Javascript, puoi utilizzare il metodo `Date()` seguito dal metodo `toLocaleDateString()` per formattare la data in una forma più leggibile per gli utenti. Dopo aver assegnato la data a una variabile, puoi stamparla utilizzando il metodo `console.log()`.

```Javascript
let currentDate = new Date();
let formattedDate = currentDate.toLocaleDateString();
console.log(formattedDate);
```

L'output potrebbe essere simile a "9/3/2021", a seconda delle impostazioni regionali del tuo browser.

In alternativa, puoi anche specificare il formato della data desiderato utilizzando i parametri del metodo `toLocaleDateString()`. Ad esempio, se vuoi stampare la data in formato "dd-mm-yyyy", puoi utilizzare il codice seguente:

```Javascript
let currentDate = new Date();
let formattedDate = currentDate.toLocaleDateString('it-IT', {
  day: 'numeric',
  month: 'numeric',
  year: 'numeric'
});
console.log(formattedDate);
```

L'output sarà "09-03-2021" se stai visualizzando questo articolo il 9 marzo 2021.

## Deep Dive

In realtà, ci sono diversi modi per ottenere la data corrente in Javascript. Oltre al metodo `Date()` e `toLocaleDateString()`, puoi anche utilizzare il metodo `getDate()` per ottenere solo il giorno, `getMonth()` per il mese e `getFullYear()` per l'anno.

Inoltre, puoi anche aggiungere o sottrarre giorni, mesi o anni dalla data corrente utilizzando i metodi `setDate()`, `setMonth()` e `setFullYear()`. Ad esempio, se vuoi ottenere la data tra due settimane, puoi utilizzare il codice seguente:

```Javascript
let currentDate = new Date();
currentDate.setDate(currentDate.getDate() + 14);
console.log(currentDate.toLocaleDateString());
```

L'output sarà la data corrente tra due settimane.

## Vedi anche

- [Documentazione di Javascript su Date()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Info su come formattare una data con il metodo `toLocaleDateString()`](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)