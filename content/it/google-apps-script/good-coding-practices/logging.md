---
title:                "Registrazione"
aliases:
- it/google-apps-script/logging.md
date:                  2024-02-01T21:56:24.808235-07:00
model:                 gpt-4-0125-preview
simple_title:         "Registrazione"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/logging.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Il logging nella programmazione comporta la registrazione di eventi, errori o occorrenze notevoli durante l'esecuzione. I programmatori lo fanno per debuggare problemi, monitorare le prestazioni e conservare un registro dei dati operativi, rendendolo fondamentale per mantenere e comprendere il comportamento del software in produzione.

## Come fare:

In Google Apps Script, il logging può essere eseguito utilizzando vari metodi, come la classe `Logger` e `console.log()`. La classe Logger è il metodo tradizionale, adatto per un semplice debugging e scopi di sviluppo. Con gli aggiornamenti recenti, `console.log()` offre maggiore flessibilità e integrazione con Stackdriver Logging, fornendo una soluzione più robusta per monitorare i tuoi Apps Scripts nella Google Cloud Platform.

**Usando Logger:**

```javascript
function logSample() {
  Logger.log('Questo è un semplice messaggio di log');
  
  var value = 5;
  Logger.log('Il valore è: %s', value); // Formattazione della stringa
}

// Per visualizzare il log:
// 1. Eseguire la funzione logSample.
// 2. Visualizza -> Log
```

**Esempio di Output del Logger:**

```
[22-04-20 10:00:00:000 PDT] Questo è un semplice messaggio di log
[22-04-20 10:00:00:001 PDT] Il valore è: 5
```

**Usando console.log():**

```javascript
function consoleLogSample() {
  console.log('Questo messaggio va a Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('Registrazione di un oggetto:', obj);
}

// I log possono essere visualizzati nella console di Google Cloud Platform (GCP) sotto Stackdriver Logging
```

**Esempio di Output di console.log():**

```
Questo messaggio va a Stackdriver Logging
Registrazione di un oggetto: {name: "Jane", role: "Developer"}
```

Passando a `console.log()` per applicazioni complesse, gli sviluppatori possono analizzare efficientemente i log utilizzando i potenti filtri e strumenti forniti da GCP, il che non è altrettanto diretto con la classe Logger tradizionale.

## Approfondimento:

Il logging in Google Apps Script è evoluto significativamente. Inizialmente, la classe `Logger` era il metodo primario per gli sviluppatori per debuggare i loro script. È semplice e sufficiente per script di base, ma manca delle capacità necessarie per le moderne applicazioni cloud, come la ricerca di log o l'analisi delle tendenze dei log nel tempo.

L'introduzione di `console.log()` ha colmato questa lacuna integrando il logging di Google Apps Script con Stackdriver Logging di Google Cloud (ora chiamato Operations Suite), fornendo una piattaforma centralizzata per il logging, il monitoraggio e il debug delle applicazioni. Ciò ha permesso non solo il logging su larga scala, ma ha anche aperto a funzionalità avanzate di gestione dei log come metriche basate sui log, analisi dei log in tempo reale e integrazione con altri servizi di Google Cloud.

Mentre `Logger` serve ancora a uno scopo per il debugging rapido e il logging in script più piccoli, l'evoluzione verso l'uso di `console.log()` riflette un cambiamento più ampio nello sviluppo di applicazioni scalabili e native per il cloud. Sottolinea l'impegno di Google nel fornire agli sviluppatori strumenti che si adattano alla complessità e alla scala delle applicazioni odierne. Tuttavia, i nuovi arrivati dovrebbero essere consapevoli della curva di apprendimento leggermente più ripida e della necessità di familiarizzare con i concetti della Google Cloud Platform. Nonostante ciò, la mossa è vantaggiosa per gli sviluppatori che cercano di sfruttare appieno le capacità del cloud. Questo allineamento con i servizi cloud fa parte di una tendenza più ampia nello sviluppo del software, sottolineando l'importanza di meccanismi di logging robusti e scalabili nell'era del cloud computing.
