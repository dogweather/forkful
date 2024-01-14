---
title:    "Javascript: Ottenere la data corrente."
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Molti sviluppatori Javascript potrebbero chiedersi perché sia importante ottenere la data corrente durante la programmazione. La risposta è semplice: la data è uno dei dati più utili e utilizzati nelle applicazioni e siti web moderni.

## Come fare
Per ottenere la data corrente in Javascript, esistono diverse opzioni. La prima è utilizzare il metodo `getDate()` dell'oggetto `Date`, che restituisce il giorno del mese corrente. Ad esempio:

```Javascript
let today = new Date();
let date = today.getDate();

console.log("Oggi è il " + date + " del mese").  // Output: Oggi è il 28 del mese
```

Un'altra opzione è utilizzare il metodo `toLocaleDateString()` in combinazione con l'opzione `locale` per ottenere la data formattata secondo le impostazioni locali. Ad esempio:

```Javascript
let today = new Date();
let date = today.toLocaleDateString('it-IT', { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' });

console.log("Oggi è " + date); // Output: Oggi è lunedì 28 giugno 2021 
```

## Approfondimento
Ottenere la data corrente in Javascript può sembrare semplice, ma è importante conoscere alcuni dettagli. Per esempio, la data ottenuta tramite i metodi citati sopra dipende dalle impostazioni della macchina del cliente, quindi potrebbe essere formattata in modo diverso da un paese all'altro. Inoltre, è possibile aggiungere o sottrarre giorni, mesi o anni dalla data ottenuta per ottenere date future o passate. È anche importante prestare attenzione al fuso orario, poiché la data corrente dipende dalla posizione geografica del dispositivo.

## Vedi anche
- [MDN - Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Dates](https://www.w3schools.com/js/js_date_methods.asp)