---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Converting a date into a string in Javascript is a common task that allows us to display dates in a specific format or manipulate them in various ways. It also allows us to work with dates in a readable format that is understandable for both human and machine.

## Come Fare

Per convertire una data in una stringa in Javascript, possiamo utilizzare il metodo `toString()` di un oggetto `Date` seguito da una serie di metodi per formattare la stringa secondo le nostre esigenze. Ad esempio:

```javascript
let today = new Date();

let dateAsString = today.toString(); // "Fri Nov 20 2020 14:15:30 GMT+0100 (Central European Standard Time)"
```

Possiamo anche utilizzare un oggetto `Intl.DateTimeFormat` per formattare la data in una specifica localizzazione. Ad esempio, se vogliamo ottenere la data attuale in formato italiano:

```javascript
let today = new Date();

let italyDate = new Intl.DateTimeFormat('it-IT').format(today); // "20/11/2020"
```

## Approfondimento

Il metodo `toString()` restituisce la data completa con orario e fuso orario nell'output. Per rimuovere queste informazioni e ottenere solo la data, possiamo utilizzare il metodo `toDateString()`:

```javascript
let today = new Date();

let dateOnly = today.toDateString(); // "Fri Nov 20 2020"
```

Inoltre, possiamo utilizzare il metodo `toLocaleDateString()` per ottenere la data in formato locale specifico, senza dover specificare il codice di localizzazione come nel caso dell'oggetto `Intl.DateTimeFormat`:

```javascript
let today = new Date();

let localeDate = today.toLocaleDateString(); // "20/11/2020" (per localizzazione italiano)
```

## Vedi Anche

- [Documentazione ufficiale di Javascript sulla gestione delle date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutorial su come lavorare con le date in Javascript](https://www.html.it/pag/53458/lavorare-con-le-date-in-javascript/)