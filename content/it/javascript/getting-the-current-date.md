---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Ottenere la data corrente in Javascript

## Cosa e Perche?
Ottenere la data corrente in Javascript significa richiedere le informazioni attuali sull'anno, il mese, il giorno e l'ora dal sistema. I programmatori lo fanno per tracciare eventi, per l'ordinamento o confronto delle date, per la visualizzazione delle date in formati diversi e molto altro.

## Come si fa:
Per ottenere la data corrente in Javascript, si utilizza l'oggetto `Date` incorporato. 

Ecco come si fa:

```Javascript
let dataCorrente = new Date(); 
console.log(dataCorrente);
```
Questo restituirà la data e l'ora corrente del sistema nell'output, qualcosa di simile a:

```Javascript 
Tue Oct 12 2021 13:31:51 GMT+0200 (Central European Summer Time)
```

## Approfondimento
L'oggetto Date in Javascript è stato originariamente progettato prendendo come modello l'oggetto Date in Java, ed è presente in Javascript dalla sua prima versione.
Ci sono diverse alternative per ottenere la data: si potrebbero utilizzare le librerie esterne come Moment.js o Day.js, che forniscono molte funzionalità in più rispetto all'oggetto Date predefinito. Tuttavia, per simple operazioni come ottenere la data corrente, l'uso di tali librerie potrebbe essere eccessivo.
Per quanto riguarda i dettagli implementativi, quando si crea un nuovo oggetto Date senza passare argomenti, Javascript ottiene la data e l'ora correnti dal clock del sistema del computer.

## Vedi Anche
Per un'ulteriore lettura sulle date e le ore in Javascript, consiglio le seguenti risorse:

- Documentazione Mozilla Developer Network sull'oggetto Date: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date
- Libreria Moment.js: https://momentjs.com/
- Libreria Day.js: https://day.js.org/