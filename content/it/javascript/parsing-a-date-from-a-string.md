---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?

Estrarre una data da una stringa significa convertire un testo (come "01/02/2003") in un oggetto ```Date``` in Javascript. Lo facciamo per manipolare e utilizzare la data in vari modi nel nostro codice, ad esempio per fare calcoli o per organizzare in modo efficace i dati temporali.

## Come fare:

Ecco un esempio su come farlo in Javascript:

```Javascript
let stringaData = "03/04/2005";
let data = new Date(stringaData);
console.log(data);
```

Questo ti restituirà un oggetto ```Date``` come:

```Javascript
Sat Apr 03 2005 00:00:00 GMT+0200 (Central European Standard Time)
```

## Panoramica

La possibilità di creare un oggetto ```Date``` da una stringa è stata introdotta in Javascript 1.0. Ma dato che ci sono molti formati di data e ora nel mondo, Javascript offre vari modi per interpretare le stringhe di data. Se la stringa non è nel formato specificato da ECMAScript (ISO 8601), Javascript proverà a interpretarla utilizzando il formato Date del browser. Questo può variare a seconda della localizzazione e del browser, quindi è più sicuro utilizzare una stringa compatibile con ISO 8601 per l'interpretazione coerente dei dati in tutte le piattaforme.

Ci sono anche librerie Javascript dedicate come Moment.js che forniscono più funzionalità per lavorare con le date e fornire parsing più robusto e preciso.

Per quanto riguarda i dettagli di implementazione, quando crei un nuovo oggetto ```Date``` con una stringa, Javascript utilizza il costruttore nativo ```Date``` per convertire la stringa in un formato di data che può usare per le operazioni di data.

## Approfondimenti

- [Documentazione completa per Moment.js](https://momentjs.com/docs/)
- [Dettagli ECMAScript sulla Date](http://www.ecma-international.org/ecma-262/11.0/index.html#sec-date-constructor)