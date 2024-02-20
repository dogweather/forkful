---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:54.212486-07:00
description: "La gestione degli errori in Google Apps Script riguarda la previsione,\
  \ l'intercettazione e la risposta alle eccezioni o errori che si verificano durante\u2026"
lastmod: 2024-02-19 22:05:02.070180
model: gpt-4-0125-preview
summary: "La gestione degli errori in Google Apps Script riguarda la previsione, l'intercettazione\
  \ e la risposta alle eccezioni o errori che si verificano durante\u2026"
title: Gestione degli errori
---

{{< edit_this_page >}}

## Cosa & Perché?

La gestione degli errori in Google Apps Script riguarda la previsione, l'intercettazione e la risposta alle eccezioni o errori che si verificano durante l'esecuzione dello script. I programmatori la implementano per proteggere gli script da fallimenti imprevisti, garantendo applicazioni più fluide e user-friendly che possono gestire o registrare gli errori con grazia senza arresti bruschi.

## Come fare:

Google Apps Script, essendo basato su JavaScript, ci permette di usare la tradizionale istruzione `try-catch` per la gestione degli errori, insieme a `finally` se è necessaria una pulizia indipendentemente dal successo o dall'errore.

```javascript
function myFunction() {
  try {
    // Codice che potrebbe generare un errore
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("La cella A1 è vuota.");
    }
    Logger.log(data);
  } catch (e) {
    // Codice per la gestione dell'errore
    Logger.log("Errore: " + e.message);
  } finally {
    // Codice di pulizia, eseguito sia in caso di errore che no
    Logger.log("Funzione completata.");
  }
}
```

Esempio di output senza errore:
```
[Valore della cella]
Funzione completata.
```

Esempio di output con un errore (assumendo che A1 sia vuota):
```
Errore: La cella A1 è vuota.
Funzione completata.
```

Google Apps Script supporta anche il lancio di errori personalizzati utilizzando l'oggetto `Error` e l'intercettazione di tipi di errore specifici se necessario. Tuttavia, l'assenza di una categorizzazione avanzata degli errori rende essenziale affidarsi ai messaggi di errore per la specificità.

## Approfondimento

Storicamente, la gestione degli errori nei linguaggi di scripting come JavaScript (e per estensione, Google Apps Script) è stata meno sofisticata rispetto ad alcuni linguaggi compilati, che offrono caratteristiche come gerarchie di eccezioni dettagliate e strumenti di debugging completi. Il modello di Google Apps Script è relativamente semplice, sfruttando il paradigma `try-catch-finally` di JavaScript. Questa semplicità è in linea con il design del linguaggio per sviluppare e distribuire rapidamente applicazioni di piccola e media scala all'interno dell'ecosistema di Google, ma a volte può limitare gli sviluppatori che si occupano di scenari di errore complessi.

In applicazioni più complesse, i programmatori spesso integrano la gestione degli errori nativa di Google Apps Script con meccanismi personalizzati di registrazione e segnalazione degli errori. Questo può includere la scrittura degli errori in un Google Sheet per l'audit o l'uso di servizi di registrazione di terze parti attraverso i Servizi di Recupero URL di Google Apps Script per inviare i dettagli degli errori fuori dall'ambiente dello script.

Sebbene Google Apps Script possa essere indietro rispetto a linguaggi come Java o C# in termini di complessità e capacità di gestione degli errori integrate, la sua integrazione con i servizi Google e la semplicità dell'approccio `try-catch-finally` lo rendono uno strumento potente per gli sviluppatori per automatizzare rapidamente compiti e creare integrazioni all'interno dell'ecosistema Google. Gli sviluppatori provenienti da altri contesti possono trovare la sfida non nel padroneggiare schemi complessi di gestione degli errori, ma nel sfruttare in modo creativo ciò che è disponibile per garantire che i loro script siano robusti e user-friendly.
