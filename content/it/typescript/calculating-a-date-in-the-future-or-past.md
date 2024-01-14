---
title:                "TypeScript: Calcolo di una data nel futuro o nel passato."
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Calcolare una data nel futuro o nel passato può essere utile in diverse situazioni, ad esempio per pianificare un evento o per effettuare calcoli finanziari.

## Come fare
Per calcolare una data nel futuro o nel passato in TypeScript, possiamo utilizzare il costruttore `Date` e i relativi metodi per manipolare il tempo. Ad esempio, se vogliamo ottenere la data di oggi, possiamo scrivere:

```TypeScript
const oggi = new Date();
console.log(oggi); // output: Wed Mar 24 2021 00:00:00 GMT+0100 (Central European Standard Time)
```

Per calcolare una data nel futuro, possiamo utilizzare il metodo `setDate()` per impostare il giorno del mese desiderato dalla data corrente, come nel seguente esempio:

```TypeScript
const futureDate = new Date();
futureDate.setDate(futureDate.getDate() + 7); // aggiunge 7 giorni alla data corrente
console.log(futureDate); // output: Wed Mar 31 2021 00:00:00 GMT+0200 (Central European Summer Time)
```

Allo stesso modo, per calcolare una data nel passato, possiamo utilizzare il metodo `setDate()` per impostare il giorno del mese desiderato, ma sottraendolo dalla data corrente:

```TypeScript
const pastDate = new Date();
pastDate.setDate(pastDate.getDate() - 7); // sottrae 7 giorni dalla data corrente
console.log(pastDate); // output: Wed Mar 17 2021 00:00:00 GMT+0200 (Central European Summer Time)
```

Possiamo anche utilizzare i metodi `setMonth()` e `setFullYear()` per impostare il mese e l'anno desiderati nelle nostre date.

## Approfondimento
Calcolare una data nel futuro o nel passato può diventare più complesso quando si deve tenere conto di eventi come i giorni festivi o i giorni bisestili. In questi casi, è possibile utilizzare librerie esterne come Moment.js che semplificano la manipolazione delle date in TypeScript.

Inoltre, è importante tenere conto della zona temporale in cui si trova il nostro utente o il nostro server per avere valori precisi nella nostra data.

## Vedi anche
- [Documentazione ufficiale di TypeScript su Date](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [Moment.js](https://momentjs.com/) - libreria per la manipolazione delle date e dei tempi in JavaScript e TypeScript.