---
date: 2024-01-26 03:49:52.314418-07:00
description: "Ecco un po' di codice JavaScript che non si comporta come previsto:\
  \ ```javascript function buggyMultiply(a, b) { return a + b; // Ops! Questo dovrebbe\u2026"
lastmod: '2024-03-13T22:44:43.816804-06:00'
model: gpt-4-0125-preview
summary: "Ecco un po' di codice JavaScript che non si comporta come previsto:\n\n\
  ```javascript\nfunction buggyMultiply(a, b) {\n    return a + b; // Ops."
title: Utilizzo di un debugger
weight: 35
---

## Come fare:
Ecco un po' di codice JavaScript che non si comporta come previsto:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Ops! Questo dovrebbe essere una moltiplicazione, non un'addizione.
}

let result = buggyMultiply(5, 3);
console.log('Risultato:', result);
```

L'output è incorretto:
```
Risultato: 8
```

Debuggiamo in Chrome DevTools:

1. Apri questo JS in un browser.
2. Fai clic destro e seleziona "Ispeziona" per aprire DevTools.
3. Clicca sulla scheda "Sorgenti".
4. Trova il tuo frammento di codice o pagina e metti un punto di interruzione cliccando sul numero di riga accanto all'istruzione `return`.
5. Aggiorna la pagina per attivare il punto di interruzione.
6. Controlla il pannello "Scope" per vedere le variabili locali `a` e `b`.
7. Prosegui con il bottone "Passa oltre la prossima chiamata di funzione".
8. Individua il bug nell'istruzione `return`.
9. Correggi il codice:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Corretto!
}

let result = buggyMultiply(5, 3);
console.log('Risultato:', result);
```

L'output corretto:
```
Risultato: 15
```

## Approfondimento
Il concetto di debugging esiste sin dai primi giorni dell'informatica—una leggenda racconta che tutto iniziò quando una falena fu trovata dentro un computer negli anni '40! Oggi, i debugger JavaScript come gli strumenti integrati nei browser (Chrome DevTools, Strumenti per Sviluppatori Firefox) o debugger integrati nell'IDE (Visual Studio Code, WebStorm) offrono una quantità di funzionalità.

Alternative agli debugger integrati includono strumenti di terze parti come WebStorm o l'uso del buon vecchio `console.log` per stampare gli stati delle variabili. Ma questi non offrono l'interazione in tempo reale e l'ispezione dettagliata fornite dai debugger.

Per quanto riguarda i dettagli di implementazione, la maggior parte dei debugger funziona in modo simile: consentono di impostare punti di interruzione che mettono in pausa l'esecuzione, di eseguire il passaggio attraverso il codice, ispezionare gli stati attuali delle variabili, osservare espressioni e persino manipolare valori al volo per testare diversi scenari.

## Vedi Anche
- [Strumenti per sviluppatori di Google Chrome](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Debugger Firefox](https://developer.mozilla.org/it/docs/Tools/Debugger)
- [Visual Studio Code - Debugging](https://code.visualstudio.com/docs/editor/debugging)
