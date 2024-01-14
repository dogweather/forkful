---
title:                "Javascript: Ottenere la data attuale"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
La funzione di ottenere la data corrente è un'importante strumento di programmazione in Javascript che consente di aggiungere dinamicità alle applicazioni web. Con questa funzione, è possibile visualizzare la data attuale sulla pagina e utilizzarla per una varietà di scopi, come ad esempio aggiornare automaticamente le informazioni in base alla data.

## Come Fare
Per ottenere la data corrente in Javascript, utilizzeremo l'oggetto Date(), che rappresenta la data e l'ora correnti. Questo oggetto fornisce vari metodi per accedere alle informazioni sulla data e sull'ora, come ad esempio getDate(), getMonth(), getFullYear(), getHours() e molti altri.

```
// Creiamo un nuovo oggetto Date
let dataCorrente = new Date();

// Otteniamo la data
let data = dataCorrente.getDate();

// Otteniamo il mese
let mese = dataCorrente.getMonth();

// Otteniamo l'anno
let anno = dataCorrente.getFullYear();

// Stampiamo la data completa
console.log(`La data di oggi è ${data}/${mese + 1}/${anno}`);
```

Questo codice restituirà la data corrente nel formato giorno/mese/anno e verrà stampato nella console come "La data di oggi è 22/12/2020".

## Approfondimento
L'oggetto Date() in Javascript rappresenta la data e l'ora secondo il fuso orario del sistema in cui viene eseguito il codice. Tuttavia, è anche possibile impostare manualmente la data e l'orario specifici utilizzando i metodi set() dell'oggetto Date. Inoltre, esistono anche librerie esterne che consentono di formattare la data in modi diversi, come ad esempio Moment.js.

## Vedi Anche
- [Documentazione sull'oggetto Date in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutorial su come utilizzare l'oggetto Date in Javascript](https://www.html.it/pag/32542/capire-javascript-date/)
- [Libreria Moment.js per la gestione delle date in Javascript](https://momentjs.com/)