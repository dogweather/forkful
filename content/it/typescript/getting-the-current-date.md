---
title:                "TypeScript: Ottenere la data corrente"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data corrente è un dato importante in molte situazioni di programmazione. Ad esempio, può essere utilizzata per registrare transazioni, generare report o semplicemente fornire informazioni aggiuntive per l'utilizzo dell'applicazione. Ottenere la data corrente in TypeScript è molto semplice e può essere fatto in diversi modi.

## Come fare

Per ottenere la data corrente in TypeScript, è possibile utilizzare l'oggetto Date. Ci sono vari metodi disponibili per ottenere ogni parte della data (anno, mese, giorno, ecc.) oltre a metodi per ottenere la data completa o solo l'ora.

Esempio di codice:

```TypeScript
// Ottenere la data completa
const currentDate = new Date();
console.log(currentDate); // Output: Mon Aug 09 2021 13:23:29 GMT+0200 (Central European Summer Time)

// Ottenere solo l'ora corrente
const currentHour = new Date().getHours();
console.log(currentHour); // Output: 13
```

## Approfondimento

L'oggetto Date in TypeScript è molto potente e offre numerosi metodi e opzioni per manipolare e ottenere la data e l'ora. Ad esempio, è possibile impostare una data specifica utilizzando il suo costruttore, o convertire una data in diversi formati utilizzando i metodi toISOString() o toLocaleString().

Esempio di codice:

```TypeScript
// Impostare una data specifica
const specificDate = new Date('2021-08-10');
console.log(specificDate); // Output: Tue Aug 10 2021 00:00:00 GMT+0200 (Central European Summer Time)

// Convertire una data in formato ISO string
const isoString = specificDate.toISOString();
console.log(isoString); // Output: 2021-08-10T00:00:00.000Z

// Convertire una data in formato localizzato
const localizedDate = specificDate.toLocaleString('it-IT');
console.log(localizedDate); // Output: 10/08/2021, 00:00:00
```

## Vedi anche

- Documentazione ufficiale di TypeScript sull'oggetto Date: https://www.typescriptlang.org/docs/handbook/standard-library.html#differences-from-javascript
- Guida su come lavorare con date e orari in TypeScript: https://www.toptal.com/software/definitive-guide-datetime-manipulation
- Esempi di utilizzo dei metodi dell'oggetto Date: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date