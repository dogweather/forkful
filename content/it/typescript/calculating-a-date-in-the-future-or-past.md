---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "TypeScript: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calcolare una data futura o passata con TypeScript

## Che cos'è & Perché?
Calcolare una data futura o passata significa manipolare un oggetto Data in TypeScript (o JavaScript) per ottenere una data specifica diversa da quella corrente. Questa funzionalità è essenziale per diverse applicazioni come la pianificazione, la gestione delle scadenze, le prenotazioni o il tracciamento del tempo.

## Come fare:
Ecco un esempio di elaborazione di una data futura e passata con TypeScript:
```TypeScript
let oggi = new Date(); //Data corrente
console.log("Oggi è: " + oggi);

let settimanaDopo = new Date(oggi.getTime() + 7 * 24 * 60 * 60 * 1000); //Una settimana dopo
console.log("Una settimana dopo: " + settimanaDopo);

let settimanaPrima = new Date(oggi.getTime() - 7 * 24 * 60 * 60 * 1000); //Una settimana prima
console.log("Una settimana prima: " + settimanaPrima);
```
Questo codice crea una nuova data per "oggi", poi calcola e stampa una data per una settimana dopo e una settimana prima di oggi.

## Approfondimento
Il concetto di calcolo delle date nel futuro o nel passato risale all'epoca dei primi linguaggi di programmazione, dove si è reso necessario per una serie di applicazioni.

Ci sono molte altre librerie per lavorare con le date in TypeScript o JavaScript, come moment.js o date-fns. Possono offrire più funzionalità o una sintassi più semplice, ma al costo di aggiungere una dipendenza esterna al tuo progetto. 

Quando si tratta di implementazione, aggiungere o sottrarre il tempo da una data in JavaScript (e quindi TypeScript) implica la conversione dei giorni / ore / minuti / secondi in millisecondi, in quanto il metodo getTime() restituisce il tempo corrente in millisecondi.

## Vedi Anche
Per un approfondimento su come lavorare con le date in TypeScript, ecco alcune risorse utili:

- [*Date* MDN Web Docs](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)

- [*Moment.js*](https://momentjs.com/)

- [*date-fns*](https://date-fns.org/)

Buon Coding!