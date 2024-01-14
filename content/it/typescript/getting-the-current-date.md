---
title:                "TypeScript: Ottenere la data attuale."
simple_title:         "Ottenere la data attuale."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si trovano spesso ad avere bisogno di ottenere la data corrente all'interno dei loro progetti. Questa operazione può essere utile per visualizzare informazioni aggiornate nel programma, per creare log o per altre funzionalità che dipendono dalla data.

## Come fare

Per ottenere la data corrente in TypeScript, è possibile utilizzare l'oggetto `Date`. Questo oggetto viene automaticamente creato all'interno del motore JavaScript ogni volta che viene eseguito un codice che richiede una data. Ecco un esempio di codice TypeScript per ottenere la data corrente e visualizzarla in console:

```TypeScript
const data = new Date();
console.log(data);
```

Esempio di output: `2021-04-30T16:19:47.819Z`

Se si desidera visualizzare la data in un formato leggibile come giorno, mese e anno, è possibile utilizzare i metodi `getDate()`, `getMonth()` e `getFullYear()`. Ad esempio:

```TypeScript
const data = new Date();
const giorno = data.getDate();
const mese = data.getMonth();
const anno = data.getFullYear();
console.log(`${giorno}-${mese}-${anno}`);
```

Esempio di output: `30-3-2021`

È anche possibile utilizzare librerie esterne come `moment.js` per formattare la data in modi diversi.

## Deep Dive

È importante notare che l'oggetto `Date` in JavaScript utilizza il fuso orario locale del computer in cui viene eseguito il codice. Ciò significa che, se il computer si trova in un fuso orario diverso rispetto a quello dell'utente finale del programma, la data potrebbe non essere corretta. Per evitare questo problema, è possibile utilizzare il metodo `toUTCString()` per ottenere la data e l'ora in formato UTC (Coordinated Universal Time).

Inoltre, l'oggetto `Date` in JavaScript ha molte altre funzionalità come il confronto tra date, l'aggiunta o la sottrazione di giorni e l'ottimizzazione delle prestazioni per la gestione di grandi quantità di date.

## Vedi anche

- Documentazione ufficiale sull'oggetto Date in TypeScript: https://www.typescriptlang.org/docs/handbook/standard-library.html#date
- Guida completa su moment.js: https://momentjs.com/docs/