---
title:                "Calcolare una data nel futuro o nel passato"
aliases:
- /it/google-apps-script/calculating-a-date-in-the-future-or-past.md
date:                  2024-02-01T21:48:56.326257-07:00
model:                 gpt-4-0125-preview
simple_title:         "Calcolare una data nel futuro o nel passato"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Calcolare una data nel futuro o nel passato consiste nel manipolare gli oggetti data per trovare date oltre o prima della data attuale, rispettivamente. I programmatori fanno ciò per compiti che vanno dall'impostazione di promemoria e date di scadenza all'analisi di tendenze dei dati basate sul tempo.

## Come fare:

In Google Apps Script, che si basa su JavaScript, è possibile manipolare le date utilizzando l'oggetto `Date`. Ecco come calcolare date nel futuro e nel passato:

### Calcolo della data futura

Per calcolare una data futura, si crea un oggetto data per la data corrente e poi si aggiunge il numero desiderato di giorni (o qualsiasi altra unità di tempo).

```javascript
// Data attuale
var today = new Date();

// Calcolo di una data 10 giorni nel futuro
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Data Futura: " + futureDate.toDateString());
```

### Calcolo della data passata

Similmente, per trovare una data nel passato, si sottraggono il numero di giorni dalla data corrente.

```javascript
// Data attuale
var today = new Date();

// Calcolo di una data 10 giorni nel passato
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Data Passata: " + pastDate.toDateString());
```

### Esempio di Output

Questo produrrebbe un output simile al seguente (assumendo che oggi sia il 15 aprile 2023):

```
Data Futura: Mar Apr 25 2023
Data Passata: Mer Apr 05 2023
```

Ricorda, l'oggetto Date in JavaScript (e quindi in Google Apps Script) regola automaticamente mesi e anni mentre aggiungi o sottrai giorni.

## Approfondimento

La manipolazione delle date utilizzando l'oggetto `Date` deriva dalle prime implementazioni di JavaScript. Nel tempo, questo approccio è generalmente rimasto consistente, fornendo un modo semplice per gli sviluppatori di gestire le date senza necessità di librerie esterne. Tuttavia, per operazioni più complesse come gli aggiustamenti dei fusi orari, o quando si lavora con dati basati su date estese, librerie come `Moment.js` o il più moderno `Luxon` potrebbero offrire più funzionalità e una gestione più semplice.

In Google Apps Script, specificamente, nonostante la disponibilità diretta e la semplicità dell'oggetto `Date`, è cruciale essere consapevoli di come i calcoli delle date possano impattare sulla prestazione dello script e sul tempo di esecuzione, specialmente in trigger guidati dal tempo o manipolazioni estensive di fogli di calcolo. Inoltre, sebbene Google Apps Script fornisca metodi integrati per gestire le date all'interno del suo ecosistema (come in Google Sheets o Calendar), integrare librerie esterne o sfruttare i Servizi Avanzati di Google può talvolta fornire soluzioni più robuste per scenari complessi.

Quindi, mentre la metodologia dell'oggetto `Date` nativo di JavaScript è solitamente sufficiente per calcoli diretti, esplorare librerie o servizi esterni può migliorare la funzionalità per requisiti più sfumati.
