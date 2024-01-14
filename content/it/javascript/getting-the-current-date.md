---
title:                "Javascript: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché 

Essere in grado di ottenere la data corrente è fondamentale per molte applicazioni web e di programmazione in generale. Ad esempio, potresti voler mostrare la data corrente sul tuo sito web o calcolare l'età di un utente in base alla loro data di nascita. Avere questa funzionalità a portata di mano ti consente di creare applicazioni più dinamiche e personalizzate.

## Come fare 

Per ottenere la data corrente in JavaScript, possiamo utilizzare l'oggetto Date. Questo oggetto contiene informazioni sulla data e sull'ora attuali e ci consente di estrarre queste informazioni in vari formati. Ecco un esempio di codice che restituirà la data corrente nel formato gg/mm/aaaa:

```Javascript
let today = new Date(); // creiamo un nuovo oggetto Date
let day = today.getDate(); // restituisce il giorno del mese (1-31)
let month = today.getMonth() + 1; // restituisce il mese (0-11)
let year = today.getFullYear(); // restituisce l'anno (4 cifre)
console.log(`${day}/${month}/${year}`); // stampiamo la data corrente
```

Il risultato dovrebbe essere qualcosa del genere: "14/05/2021".

Possiamo anche modificare il formato della data utilizzando i metodi forniti dall'oggetto Date. Ad esempio, se vogliamo ottenere la data in formato mm/gg/aaaa, possiamo sostituire la riga di codice per ottenere il mese con la seguente:

```Javascript
let month = today.getMonth() + 1;
```

In questo modo, il risultato sarà "05/14/2021".

## Approfondimento 

Se sei interessato a saperne di più sul funzionamento dell'oggetto Date e su come gestire le date in JavaScript, ecco alcune cose che potresti voler esplorare:

- Come gestire i fusi orari: l'oggetto Date tiene conto del fuso orario corrente del computer in cui viene eseguito il codice. Tuttavia, puoi anche impostare un fuso orario specifico utilizzando i metodi `setTime()` e `getTimezoneOffset()`.
- Come calcolare il numero di giorni in un determinato mese: l'oggetto Date ha metodi utili per ottenere informazioni sul numero di giorni in un determinato mese, poiché questo può variare a seconda del mese e dell'anno.
- Come creare un timer o un conto alla rovescia: utilizzando l'oggetto Date, è possibile impostare un timer o un conto alla rovescia per una determinata data e ora.
- Come formattare la data in modo più dettagliato: l'oggetto Date ha anche metodi per ottenere informazioni più specifiche sulla data, come il nome del giorno della settimana o il nome del mese.

## Vedi anche 

Se vuoi approfondire ulteriormente il tema delle date in JavaScript, qui ci sono alcuni link utili:

- [W3Schools: JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)
- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript.info: Date and time](https://javascript.info/date)