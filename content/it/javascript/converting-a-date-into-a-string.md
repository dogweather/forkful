---
title:    "Javascript: Convertire una data in una stringa"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per voler convertire una data in una stringa mediante programmazione. Ad esempio, potresti voler visualizzare una data in un formato specifico, come 10/03/2021 o Marzo 10, 2021 per rendere più comprensibili le informazioni per gli utenti. Inoltre, la conversione in una stringa può essere utile per confrontare le date o per eseguire operazioni di calcolo tra date diverse.

## Come Fare
Per convertire una data in una stringa in Javascript, puoi utilizzare il metodo `toString()` dell'oggetto `Date`. Vediamo un esempio pratico:

```javascript
let data = new Date(); //crea un oggetto data con la data odierna
let stringaData = data.toString();
console.log(stringaData); //stamperà la data in formato stringa
```

L'output di questo codice sarà simile a: "Wed Mar 10 2021 09:37:40 GMT+0100 (Central European Standard Time)". Tuttavia, potresti voler il formato della data in un modo diverso. Per farlo, puoi utilizzare i metodi `getDate()`, `getMonth()` e `getFullYear()` dell'oggetto `Date` per ottenere rispettivamente il giorno, il mese e l'anno della data, e formattarli in una stringa desiderata. Ecco un esempio:

```javascript
let data = new Date();
let giorno = data.getDate();
let mese = data.getMonth() + 1; //i mesi iniziano da 0, quindi si aggiunge 1
let anno = data.getFullYear();

let stringaData = `${giorno}/${mese}/${anno}`;
console.log(stringaData); //stamperà la data in formato gg/mm/aaaa
```

L'output di questo codice sarà: "10/03/2021".

## Approfondimento
È importante notare che la conversione di una data in una stringa può variare in base alla lingua e alla località in cui viene eseguita. Ad esempio, in Italia il giorno viene indicato prima del mese, mentre in alcuni paesi anglofoni il mese viene indicato prima del giorno. Inoltre, alcuni metodi dell'oggetto `Date` possono restituire numeri con un solo cifra, come nel caso del mese, mentre altri ritornano numeri con più cifre, come nell'esempio dell'anno. Ciò può influire sulla formattazione della stringa finale.

Per ulteriori informazioni sulla gestione delle date in Javascript, puoi consultare la documentazione ufficiale o altri tutorial online.

## Vedi Anche
- La documentazione ufficiale di Javascript per la gestione delle date: [https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Tutorial su come formattare una data in Javascript: [https://www.w3schools.com/js/js_date_formats.asp](https://www.w3schools.com/js/js_date_formats.asp)
- Altri approfondimenti sulla gestione delle date in Javascript: [https://www.toptal.com/software/definitive-guide-to-datetime-manipulation](https://www.toptal.com/software/definitive-guide-to-datetime-manipulation)