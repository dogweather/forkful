---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:26.341643-07:00
description: "Il debugging in Google Apps Script (GAS) comporta il processo di identificazione\
  \ ed eliminazione di errori dagli script destinati ad automatizzare Google\u2026"
lastmod: '2024-02-25T18:49:40.890067-07:00'
model: gpt-4-0125-preview
summary: "Il debugging in Google Apps Script (GAS) comporta il processo di identificazione\
  \ ed eliminazione di errori dagli script destinati ad automatizzare Google\u2026"
title: Utilizzo di un debugger
---

{{< edit_this_page >}}

## Cosa e perché?

Il debugging in Google Apps Script (GAS) comporta il processo di identificazione ed eliminazione di errori dagli script destinati ad automatizzare Google Apps o costruire applicazioni web. I programmatori effettuano il debug per assicurarsi che il loro codice venga eseguito come previsto, migliorando affidabilità e prestazioni nelle applicazioni.

## Come fare:

Google Apps Script fornisce un debugger integrato all'interno dell'Editor di Apps Script per aiutare a risolvere i problemi degli script. Ecco come avviare e utilizzare il debugger:

1. **Apri il tuo script nell'Editor di Apps Script.**
2. **Seleziona una funzione da esaminare.** Dal menu a tendina in alto, seleziona la funzione che desideri esaminare.
3. **Imposta i breakpoint.** Clicca sulla gutter (l'area grigia a sinistra dei numeri di riga) dove desideri interrompere l'esecuzione; apparirà un puntino rosso, indicando un breakpoint.
4. **Inizia a esaminare.** Clicca sull'icona del bug o seleziona `Debug` > `Inizia esaminazione`. L'esecuzione inizierà e si interromperà al primo breakpoint.

Considera questo semplice script:

```javascript
function calcolaSomma() {
  var a = 5;
  var b = 10;
  var somma = a + b;
  Logger.log(somma); // Intenzionato a registrare 15
}
```

Se non sei sicuro del perché `Logger.log(somma)` non stia visualizzando il risultato atteso, potresti impostare un breakpoint alla riga `var somma = a + b;` e passare attraverso lo script riga per riga per ispezionare i valori delle variabili.

**Output di esempio nel Logger:**

```plain
15
```

Durante il debug, l'Editor di Apps Script consente di:

- **Passare attraverso il codice** usando i pulsanti passo oltre, passo dentro e passo fuori.
- **Osservare espressioni e variabili** per vedere i loro valori cambiare in tempo reale.
- **Ispezionare lo stack delle chiamate** per rintracciare le chiamate di funzione.

## Approfondimento

Il debugging in Google Apps Script, come in qualsiasi altro ambiente di programmazione, è essenziale per creare applicazioni prive di errori. Introdotta agli inizi dello sviluppo di GAS, il debugger integrato offre capacità fondamentali per ispezionare e correggere il codice incrementalmente. Sebbene fornisca funzionalità di base di debug simili a quelle trovate in ambienti più maturi come Visual Studio Code o IntelliJ, potrebbe risultare insufficiente per scenari di debug complessi. Ad esempio, le sue capacità di ispezionare callback asincroni o gestire esecuzioni di script pesanti potrebbero essere limitanti.

Per esigenze di debug complesse, gli sviluppatori potrebbero ricorrere a metodi alternativi come la registrazione estensiva (usando `Logger.log()`) o persino il dispiegamento come app web per ispezionare il comportamento in uno scenario reale. Tuttavia, la semplicità e l'integrazione del debugger di GAS nell'Editor di Apps Script lo rendono un primo passo inestimabile per la risoluzione dei problemi e la comprensione del comportamento degli script. Notabilmente, con gli aggiornamenti continui e i miglioramenti di Google a Apps Script, l'esperienza di debug è in costante miglioramento, offrendo strumenti e opzioni più sofisticate nel tempo. Questa evoluzione riflette l'impegno di Google a rendere Apps Script una piattaforma più potente e accessibile per sviluppatori di diversi background.
