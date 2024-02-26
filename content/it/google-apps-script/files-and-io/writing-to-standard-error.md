---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:04.693879-07:00
description: "La scrittura su standard error (stderr) nei linguaggi di programmazione\
  \ riguarda l'indirizzamento dei messaggi di errore e delle diagnostiche su un flusso\u2026"
lastmod: '2024-02-25T18:49:40.902785-07:00'
model: gpt-4-0125-preview
summary: "La scrittura su standard error (stderr) nei linguaggi di programmazione\
  \ riguarda l'indirizzamento dei messaggi di errore e delle diagnostiche su un flusso\u2026"
title: Scrivere sull'errore standard
---

{{< edit_this_page >}}

## Cosa & Perché?

La scrittura su standard error (stderr) nei linguaggi di programmazione riguarda l'indirizzamento dei messaggi di errore e delle diagnostiche su un flusso separato, diverso dall'output standard (stdout). I programmatori fanno ciò per distinguere l'output normale del programma dai messaggi di errore, rendendo il debug e l'analisi dei log più semplici.

## Come fare:

Google Apps Script, essendo un linguaggio di scripting per lo sviluppo di applicazioni leggere sulla piattaforma Google Apps, non fornisce una funzione incorporata diretta come `console.error()` per scrivere su stderr, come si potrebbe trovare in Node.js o Python. Tuttavia, è possibile simulare questo comportamento utilizzando i servizi di registrazione di Google Apps Script o la gestione personalizzata degli errori per gestire e separare gli output degli errori.

### Esempio: Utilizzo di `Logger` per i Messaggi di Errore

```javascript
function logError() {
  try {
    // Simula un errore
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Tentativo di divisione per zero");
  } catch (e) {
    // Scrive il messaggio di errore nei Log
    Logger.log('Errore: ' + e.message);
  }
}
```

Quando esegui `logError()`, questo scriverà il messaggio di errore nel log di Google Apps Script, che puoi visualizzare tramite `Visualizza > Log`. Questo non è esattamente stderr, ma serve uno scopo simile di separare i log degli errori dagli output standard.

### Registrazione Diagnostica Avanzata

Per un debug e una registrazione degli errori più avanzati, è possibile utilizzare Stackdriver Logging, ora noto come Google Cloud's Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // Provoca un errore deliberatamente
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Errore incontrato: ', e.toString());
  }
}
```

Questo indirizzerà il messaggio di errore a Stackdriver Logging, dove è gestito come un log di livello di errore. Da notare che l'integrazione Stackdriver/Google Cloud’s Operations Suite offre una soluzione di registrazione più granulare e ricercabile rispetto a `Logger`.

## Approfondimento

La mancanza di un flusso `stderr` dedicato in Google Apps Script riflette la sua natura e le sue origini come linguaggio di scripting basato su cloud, dove output tradizionali di console o terminali (come stdout e stderr) sono meno rilevanti. Storicamente, Google Apps Script è stato progettato per migliorare la funzionalità di Google Apps con script semplici, concentrandosi sulla facilità d'uso piuttosto che sulle funzionalità complete disponibili in ambienti di programmazione più complessi.

Detto questo, l'evoluzione di Google Apps Script verso lo sviluppo di applicazioni più sofisticate ha spinto gli sviluppatori ad adottare approcci creativi per la gestione degli errori e la registrazione, utilizzando servizi disponibili come Logger e integrando con la Google Cloud’s Operations Suite. Questi metodi, sebbene non siano implementazioni dirette di stderr, offrono alternative robuste per la gestione degli errori e la registrazione diagnostica in un ambiente incentrato sul cloud.

In modo critico, mentre questi metodi servono lo scopo all'interno dell'ecosistema di Google Apps Script, sottolineano le limitazioni della piattaforma rispetto agli ambienti di programmazione tradizionali. Per gli sviluppatori che richiedono strategie di gestione degli errori dettagliate e gerarchiche, l'integrazione con servizi di registrazione esterni o l'adozione di Google Cloud Functions, che offrono una gestione più convenzionale di stderr e stdout, potrebbe essere preferibile.
