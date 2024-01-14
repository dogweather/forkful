---
title:                "Javascript: Convertire una data in una stringa"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Convertire una data in una stringa può sembrare un'operazione molto semplice nelle applicazioni JavaScript, tuttavia è un processo fondamentale per la corretta gestione delle date e la visualizzazione di informazioni per gli utenti. In questo articolo esploreremo come effettuare questa conversione in modo efficace e accurato.

## Come fare
Per convertire una data in una stringa in JavaScript, possiamo utilizzare il metodo `toString()` che fa parte dell'oggetto `Date`. Questo metodo converte la data nella stringa seguente: `Day Mon dd yyyy HH:mm:ss GMT+0000 (Timezone)`.

Ad esempio, se vogliamo convertire la data 15/07/2021 in una stringa, possiamo utilizzare il seguente codice:

```Javascript
let data = new Date(2021, 6, 15); //la data deve essere indicata nel formato aaaa, mm, gg
let dateString = data.toString();
console.log(dateString); //Output: Thu Jul 15 2021 00:00:00 GMT+0200 (Central European Summer Time)
```

Notare che il mese viene rappresentato con un numero da 0 a 11, quindi il numero 6 indica il mese di luglio. Inoltre, dobbiamo ricordare che il metodo `toString()` restituisce la data in base alla timezone del sistema, quindi il risultato potrebbe variare in base al fuso orario.

Possiamo anche personalizzare il formato della stringa utilizzando i metodi `getDay()`, `getMonth()` e `getFullYear()` dell'oggetto `Date` per ottenere le informazioni sulla data e combinarle secondo le nostre esigenze. Ad esempio:

```Javascript
let data = new Date(2021, 6, 15);
let day = data.getDay();
let month = data.getMonth() + 1; //per ottenere il mese corretto dobbiamo aggiungere 1
let year = data.getFullYear();
let dateString = day + "/" + month + "/" + year;
console.log(dateString); //Output: 15/7/2021
```

## Approfondimento
Abbiamo visto come convertire una data in una stringa utilizzando il metodo `toString()` e personalizzando il formato della stringa usando i metodi dell'oggetto `Date`. Tuttavia, è importante tenere presente che in JavaScript le date possono essere trattate come oggetti e le operazioni su di esse possono essere complesse.

Alcune cose da tenere a mente quando si lavora con le date in JavaScript:

- Le date sono soggette alla timezone del sistema, quindi potrebbero variare in base alla configurazione del fuso orario.
- Quando si crea un oggetto `Date`, se non si specifica un orario, verrà impostato automaticamente alla mezzanotte (00:00:00).
- Le date possono essere confrontate utilizzando gli operatori logici, tuttavia le operazioni matematiche su di esse possono essere più complesse a causa della struttura dei dati delle date in JavaScript (basata su millisecondi).
- Per operazioni più complesse come l'aggiunta o la sottrazione di giorni, mesi o anni, possono essere utilizzate librerie esterne come moment.js.

## Vedi anche
- [Documentazione ufficiale di JavaScript sul metodo toString()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Moment.js](https://momentjs.com/)