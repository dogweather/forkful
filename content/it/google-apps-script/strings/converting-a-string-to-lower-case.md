---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:04.659748-07:00
description: "Convertire una stringa in minuscolo in Google Apps Script, un linguaggio\
  \ di scripting basato su cloud per automatizzare compiti attraverso i prodotti\u2026"
lastmod: '2024-03-13T22:44:42.940293-06:00'
model: gpt-4-0125-preview
summary: "Convertire una stringa in minuscolo in Google Apps Script, un linguaggio\
  \ di scripting basato su cloud per automatizzare compiti attraverso i prodotti\u2026"
title: Convertire una stringa in minuscolo
---

{{< edit_this_page >}}

## Cosa & Perché?

Convertire una stringa in minuscolo in Google Apps Script, un linguaggio di scripting basato su cloud per automatizzare compiti attraverso i prodotti Google, è un'operazione fondamentale volta a standardizzare i dati testuali. I programmatori spesso eseguono questa azione per garantire la coerenza nell'input degli utenti, nell'elaborazione dei dati o nel confronto delle stringhe, poiché elimina i problemi legati alla sensibilità per il maiuscolo/minuscolo.

## Come fare:

Convertire una stringa in minuscolo in Google Apps Script è semplice, grazie ai metodi JavaScript incorporati disponibili nell'ambiente di scripting. Il metodo `toLowerCase()` è quello che userai maggiormente. Ecco come puoi implementarlo:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // Outputs: hello, world!
}
```

Questa semplice funzione dimostra di prendere una stringa originale, applicare il metodo `toLowerCase()`, e registrare il risultato. Questo è particolarmente utile quando si gestiscono input che devono essere insensibili al caso. Ad esempio, nel confronto di indirizzi email che gli utenti potrebbero inserire in vari casi.

Inoltre, per situazioni in cui stai lavorando con dati array, puoi mappare ciascun elemento per convertirli in minuscolo:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // Outputs: [alice, bob, charlie]
}
```

Questo esempio sottolinea la versatilità di `toLowerCase()` quando si gestiscono più dati di stringa, garantendo uniformità in tutto il tuo set di dati.

## Approfondimento

Il metodo `toLowerCase()`, ereditato da JavaScript e utilizzato all'interno di Google Apps Script, è stato una parte integrante della manipolazione delle stringhe sin dalle prime versioni di JavaScript. Il suo scopo principale è quello di aiutare nella gestione dei dati testuali insensibili al maiuscolo/minuscolo, una necessità sorta con l'avvento delle applicazioni web dinamiche e interattive. Nonostante la sua semplicità, il meccanismo svolge un ruolo cruciale nella validazione dei dati, nell'ordinamento e negli algoritmi di ricerca riducendo la complessità introdotta dalla sensibilità al caso.

In termini di prestazioni, il processo di conversione è altamente ottimizzato nei moderni motori JavaScript; tuttavia, la sua applicazione dovrebbe essere ancora giudiziosa all'interno di operazioni su larga scala per evitare sovraccarichi di elaborazione non necessari.

Un'alternativa da considerare, specialmente quando si lavora con pattern complessi o si necessita di conversioni specifiche per locale, è il metodo `toLocaleLowerCase()`. Questa variante considera le regole specifiche del locale per convertire i caratteri in minuscolo, ciò che potrebbe essere essenziale per applicazioni che supportano più lingue:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // Outputs: märz
```

Nonostante la complessità aggiuntiva, `toLocaleLowerCase()` è uno strumento potente per le applicazioni internazionali, garantendo che la conversione rispetti le norme linguistiche del locale dell'utente. Qualunque metodo tu scelga, convertire le stringhe in minuscolo rimane una parte essenziale del processo di testo in Google Apps Script, colmando il divario tra input dell'utente e gestione standardizzata dei dati.
