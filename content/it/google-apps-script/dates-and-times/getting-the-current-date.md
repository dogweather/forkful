---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:24.671694-07:00
description: "Ottenere la data corrente in Google Apps Script riguarda il recupero\
  \ della data e dell'ora attuali, un compito comune per l'automazione di compiti,\
  \ la\u2026"
lastmod: '2024-03-11T00:14:16.519907-06:00'
model: gpt-4-0125-preview
summary: "Ottenere la data corrente in Google Apps Script riguarda il recupero della\
  \ data e dell'ora attuali, un compito comune per l'automazione di compiti, la\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## Cosa e Perché?

Ottenere la data corrente in Google Apps Script riguarda il recupero della data e dell'ora attuali, un compito comune per l'automazione di compiti, la registrazione e la marcatura temporale nelle app legate all'ecosistema di Google. I programmatori utilizzano questa funzionalità per la generazione di contenuti dinamici, il monitoraggio delle scadenze e la pianificazione all'interno di Google Docs, Fogli e altri servizi Google.

## Come fare:

Google Apps Script, che si basa su JavaScript, offre metodi semplici per ottenere la data corrente. Puoi usare il costruttore `new Date()` per creare un nuovo oggetto data che rappresenta la data e l'ora correnti. Ecco come puoi manipolare e visualizzare questo in vari formati.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Registra la data e l'ora corrente nel fuso orario dello script
  
  // Per visualizzare solo la data nel formato AAAA-MM-GG
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Esempio di output: "2023-04-01"
  
  // Visualizzazione in un formato più leggibile
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Esempio di output: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

Questi frammenti dimostrano come catturare e formattare la data e l'ora corrente, mostrando la versatilità per vari bisogni di programmazione all'interno di Google Apps Script.

## Approfondimento

Prima che JavaScript si stabilizzasse sull'oggetto `Date`, i programmatori dovevano manualmente tenere traccia del tempo e della data attraverso mezzi meno standard e più ingombranti. Ciò includeva l'uso di interi timestamp e funzioni di data fatte in casa, che variavano da un ambiente di programmazione all'altro, portando a incongruenze e problemi di compatibilità.

L'introduzione dell'oggetto `new Date()` in JavaScript, e per estensione in Google Apps Script, ha standardizzato le operazioni di data e ora, rendendole più intuitive e riducendo la quantità di codice necessario per le operazioni legate alla data. È degno di nota che, sebbene l'implementazione di Google Apps Script sia conveniente e sufficiente per molte applicazioni all'interno della suite di prodotti Google, potrebbe non soddisfare tutti gli scenari, in particolare quelli che richiedono una gestione complessa dei fusi orari o una registrazione precisa dei timestamp in ambienti frenetici.

Per tali casi d'uso avanzati, i programmatori spesso si rivolgono a librerie come Moment.js o date-fns in JavaScript. Sebbene Google Apps Script non supporti nativamente queste librerie, gli sviluppatori possono imitare alcune delle loro funzionalità utilizzando i metodi della data JavaScript disponibili o accedendo a librerie esterne tramite il Servizio HTML o il servizio di recupero URL di Apps Script. Nonostante queste alternative, la semplicità e l'integrazione delle funzioni native di data e ora di Google Apps Script rimangono un punto di riferimento per la maggior parte dei compiti nell'ecosistema Google.
