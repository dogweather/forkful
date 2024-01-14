---
title:    "Javascript: Calcolare una data nel futuro o nel passato."
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in situazioni come la pianificazione di eventi o il monitoraggio di scadenze. Inoltre, può essere una buona opportunità per esercitarsi con le conoscenze di base di programmazione.

## Come fare

```Javascript
// Calcolo della data futura
let oggi = new Date(); // Ottiene la data di oggi
let unMeseDopo = new Date(oggi.getFullYear(), oggi.getMonth()+1, oggi.getDate()); // Ottiene la data di oggi più un mese
console.log(unMeseDopo.toLocaleDateString()); // Output: 7/4/2021

// Calcolo della data passata
let oggi = new Date(); // Ottiene la data di oggi
let unAnnoPrima = new Date(oggi.getFullYear()-1, oggi.getMonth(), oggi.getDate()); // Ottiene la data di oggi meno un anno
console.log(unAnnoPrima.toLocaleDateString()); // Output: 7/4/2020
```

Utilizzando il costruttore "Date" e i metodi "getFullYear()", "getMonth()" e "getDate()", possiamo facilmente ottenere la data scegliendo l'anno, il mese e il giorno desiderati. È importante notare che i mesi inizia dall'indice 0, quindi dobbiamo aggiungere o sottrarre 1 a seconda del caso.

## Approfondimento

Ci sono diversi metodi che possiamo utilizzare per calcolare una data nel futuro o nel passato, dalla semplice operazione di aggiunta o sottrazione di giorni, mesi o anni, alla conversione dei timestamp in date e viceversa. Inoltre, possiamo anche utilizzare librerie esterne, come moment.js, per semplificare ulteriormente il processo di calcolo delle date.

Inoltre, dobbiamo considerare che nei diversi fusi orari, la data e l'ora possono essere diverse. Pertanto, è importante tenere conto di ciò durante l'esecuzione dei calcoli.

## Vedi anche

- [Documentazione ufficiale di Javascript sul costruttore Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js: libreria per la manipolazione delle date in Javascript](https://momentjs.com/)
- [Tutorial su come calcolare date in Javascript](https://www.w3schools.com/js/js_dates.asp)