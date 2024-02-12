---
title:                "Rifattorizzazione"
aliases:
- it/google-apps-script/refactoring.md
date:                  2024-02-01T21:59:33.361706-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rifattorizzazione"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Il refactoring nel lessico della programmazione si riferisce al processo di ristrutturazione del codice informatico esistente—modificando la fattorizzazione senza cambiare il suo comportamento esterno—per migliorare attributi non funzionali. È un passo vitale per i programmatori per potenziare la leggibilità del codice, ridurre la complessità e potenzialmente scoprire bug latenti, favorendo una manutenzione più semplice e una futura scalabilità del codice.

## Come Fare:

In Google Apps Script, uno scenario comune che beneficia del refactoring è la semplificazione di script ingombranti che interagiscono con Google Sheets o Docs. Inizialmente, gli script potrebbero essere scritti in modo rapido e sommario per ottenere risultati velocemente. Con il tempo, man mano che lo script cresce, diventa ingombrante. Vediamo un esempio di refactoring per una migliore leggibilità ed efficienza.

**Script Originale:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Questa funzione registra il nome di ogni foglio in un Foglio Google. Sebbene funzioni bene, impiega pratiche JavaScript datate e manca di chiarezza.

**Script Ristrutturato:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

Nella versione ristrutturata, abbiamo passato all'uso di `const` per le variabili che non cambiano, rendendo la nostra intenzione più chiara. Abbiamo anche utilizzato il metodo `forEach`, un approccio più moderno e conciso per iterare sugli array, migliorando la leggibilità.

**Output di Esempio (per entrambi gli script):**

L'output in Logger assomiglierà a qualcosa del genere, assumendo che il tuo documento Google Sheets abbia due fogli denominati "Spese" e "Entrate":

```
[20-04-2023 10:00:00: INFO] Spese
[20-04-2023 10:00:01: INFO] Entrate
```

Lo script ristrutturato ottiene lo stesso risultato ma è più pulito e facile da capire a colpo d'occhio.

## Approfondimento

Il refactoring in Google Apps Script eredita in parte i suoi principi dalla pratica più ampia di ingegneria del software. È diventato più riconosciuto e strutturato come concetto alla fine degli anni '90, in particolare grazie al libro fondamentale di Martin Fowler "Refactoring: Improving the Design of Existing Code" (1999), che ha fornito una guida completa a varie tecniche di refactoring. Mentre le specifiche del refactoring possono variare tra i linguaggi di programmazione a causa delle loro differenze sintattiche e funzionali, l'obiettivo principale rimane lo stesso: migliorare il codice senza alterarne il comportamento esterno.

Nel contesto di Google Apps Script, un aspetto chiave da considerare durante il refactoring sono le quote di servizio e le limitazioni imposte da Google. Un codice efficientemente ristrutturato non solo è più leggibile, ma funziona anche più rapidamente e affidabilmente all'interno di questi vincoli. Per esempio, le operazioni batch (`Range.setValues()` invece di impostare i valori una cella alla volta) possono ridurre significativamente il tempo di esecuzione e il consumo di quote.

È importante notare, tuttavia, che per certi progetti complessi, Google Apps Script potrebbe risultare inadeguato a causa di queste stesse limitazioni. In tali casi, esplorare alternative come Google Cloud Functions o il nuovo fratello di Apps Script, AppSheet, potrebbe offrire una migliore scalabilità e funzionalità.

In definitiva, mentre il refactoring è un'abilità critica nel mantenere e migliorare i progetti di Google Apps Script, comprendere le limitazioni dell'ambiente e considerare soluzioni alternative è altrettanto importante per consegnare codice efficiente, robusto e mantenibile.
