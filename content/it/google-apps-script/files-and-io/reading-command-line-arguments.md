---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:51.591440-07:00
description: "Leggere gli argomenti della riga di comando in Google Apps Script \xE8\
  \ un po' improprio perch\xE9, a differenza delle interfacce a riga di comando tradizionali\u2026"
lastmod: '2024-03-13T22:44:42.972992-06:00'
model: gpt-4-0125-preview
summary: "Leggere gli argomenti della riga di comando in Google Apps Script \xE8 un\
  \ po' improprio perch\xE9, a differenza delle interfacce a riga di comando tradizionali\u2026"
title: Leggere gli argomenti della riga di comando
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere gli argomenti della riga di comando in Google Apps Script è un po' improprio perché, a differenza delle interfacce a riga di comando tradizionali nei linguaggi di programmazione come Python o Node.js, Google Apps Script non supporta intrinsecamente l'esecuzione da riga di comando o l'analisi degli argomenti. Invece, i programmatori spesso simulano questo processo attraverso funzioni personalizzate e parametri URL quando eseguono applicazioni web o compiti automatizzati, consentendo un'interazione dinamica con le funzionalità degli script in base agli input degli utenti o ai parametri predefiniti.

## Come fare:

Per emulare il processo di lettura degli argomenti della riga di comando in Google Apps Script, in particolare per le applicazioni web, puoi utilizzare i parametri della stringa di query. Quando un utente accede all'URL dell'applicazione web, puoi aggiungere argomenti come `?name=John&age=30` e analizzarli all'interno del tuo codice Apps Script. Ecco come potresti impostarlo:

```javascript
function doGet(e) {
  var params = e.parameter; // Recupera i parametri della stringa di query
  var name = params['name']; // Ottiene il parametro 'name'
  var age = params['age']; // Ottiene il parametro 'age'

  // Esempio di output:
  var output = "Nome: " + name + ", Età: " + age;
  return HtmlService.createHtmlOutput(output);
}

// URL di esempio: https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

Quando accedi all'URL con i parametri specificati, lo script produce qualcosa come:

```
Nome: John, Età: 30
```

Questo approccio è strumentale per creare interazioni personalizzate nelle applicazioni web o per controllare a livello di programmazione l'esecuzione degli script.

## Approfondimento

Gli argomenti della riga di comando, come compresi nel contesto dei linguaggi di programmazione tradizionali, portano alle capacità per gli script e le applicazioni di elaborare parametri di runtime, consentendo così esecuzioni di codice flessibili e dinamiche in base all'input dell'utente o ai processi automatizzati. Google Apps Script, essendo un linguaggio di scripting basato su cloud per lo sviluppo di applicazioni leggere nell'ecosistema di Google Workspace, non opera nativamente tramite un'interfaccia a riga di comando. Invece, la sua esecuzione è in gran parte guidata da eventi o attivata manualmente attraverso l'interfaccia utente di Apps Script e Google Workspace, o tramite applicazioni web che possono analizzare i parametri URL come pseudo argomenti della riga di comando.

Data questa differenza architettonica, i programmatori provenienti da un background di linguaggi fortemente orientati alla CLI potrebbero aver bisogno di aggiustare il loro approccio quando automatizzano compiti o sviluppano applicazioni in Google Apps Script. Invece del classico parsing degli argomenti della riga di comando, sfruttare la funzionalità dell'applicazione web di Google Apps Script o persino le funzioni personalizzate di Google Sheets per l'elaborazione interattiva dei dati può servire a fini simili. Sebbene ciò possa sembrare una limitazione inizialmente, incoraggia lo sviluppo di interfacce più intuitive e di applicazioni web accessibili, allineandosi con l'obiettivo di Google Apps Script di integrare e estendere senza problemi le applicazioni di Google Workspace.

Per scenari in cui l'emulazione più stretta del comportamento della CLI è fondamentale (ad es., automatizzare compiti con parametri dinamici), gli sviluppatori potrebbero esplorare il ricorso a piattaforme esterne che chiamano applicazioni web di Google Apps Script, passando parametri tramite URL come un metodo "da riga di comando" improvvisato. Tuttavia, per i progetti nativi di Google Apps Script, abbracciare il modello guidato da eventi e incentrato sull'interfaccia utente della piattaforma spesso conduce a soluzioni più semplici e mantenibili.
