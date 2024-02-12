---
title:                "Stampa dell'output di debug"
aliases:
- /it/google-apps-script/printing-debug-output.md
date:                  2024-02-01T21:57:56.257025-07:00
model:                 gpt-4-0125-preview
simple_title:         "Stampa dell'output di debug"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?

Stampare l'output di debug comporta il posizionamento strategico di istruzioni di log nel codice per visualizzare i valori delle variabili, il flusso di esecuzione o gli errori dei messaggi durante l'esecuzione. I programmatori lo utilizzano ampiamente per tracciare e diagnosticare il comportamento dei loro script, garantendo correttezza ed efficienza nelle loro applicazioni Google Apps Script.

## Come fare:

Google Apps Script fornisce la classe `Logger` per il debug di base e, per esigenze più avanzate, la classe `console` introdotta nell'ambiente di esecuzione V8.

**Utilizzando Logger:**

La classe Logger ti consente di registrare messaggi di debug, che puoi visualizzare dopo l'esecuzione nell'Editor di Apps Script in `Visualizza > Log`. Ecco un semplice esempio:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Ciao, %s!", name);
}
```

Dopo aver eseguito `logSample()`, puoi visualizzare il log con "Ciao, Wired Reader!" nel visualizzatore di Log.

**Utilizzando console.log con l'ambiente di esecuzione V8:**

Con l'ambiente di esecuzione V8, `console.log` fornisce una sintassi più familiare per gli sviluppatori provenienti da altri linguaggi:

```javascript
function consoleSample() {
  var status = 'attivo';
  var count = 150;
  console.log(`Stato attuale: ${status}, Conte: ${count}`);
}
```

Dopo l'esecuzione, accedi al Registro di Stackdriver in `Visualizza > Registro di Stackdriver` per visualizzare l'output. È più potente, supporta l'interpolazione di stringhe e l'ispezione degli oggetti, e si integra con il logging di Google Cloud, offrendo log persistenti e capacità di filtraggio avanzate.

**Esempio di output da console.log:**

```
Stato attuale: attivo, Conte: 150
```

## Approfondimento

Inizialmente, `Logger.log` era lo strumento primario per il debug in Google Apps Script, offrendo un modo semplice e diretto per stampare l'output per l'ispezione. Tuttavia, man mano che gli script diventavano più complessi e integrati con i servizi di Google Cloud Platform, è diventata evidente la necessità di una soluzione di logging più robusta.

Entra in gioco l'ambiente di esecuzione V8, introducendo `console.log`. Ciò non solo allinea Google Apps Script con la sintassi JavaScript standard, rendendo il linguaggio più accessibile agli sviluppatori familiarizzati con JavaScript, ma sfrutta anche l'infrastruttura potente delle capacità di logging di Google Cloud. L'introduzione di `console.log` e la sua integrazione con Google Cloud Platform segnano una significativa evoluzione nelle capacità di debug dentro Google Apps Script, fornendo agli sviluppatori un approccio più dinamico e scalabile per monitorare e risolvere i problemi dei loro script.

Mentre `Logger.log` è sufficiente per le esigenze di debug di base e i piccoli progetti, `console.log` con l'ambiente di esecuzione V8 offre una soluzione più completa e a prova di futuro. Questo include la capacità di conservare i log oltre la sessione di esecuzione, cercare e filtrare i log all'interno della console di Google Cloud, e l'allineamento generale con le pratiche di sviluppo JavaScript moderno. Tuttavia, gli sviluppatori dovrebbero valutare le loro esigenze rispetto alla complessità e alla scala dei loro progetti quando scelgono tra queste opzioni.
