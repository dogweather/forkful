---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:31.470197-07:00
description: "L'interpolazione di stringhe in Google Apps Script consente l'incorporamento\
  \ dinamico di espressioni all'interno delle stringhe, facilitando la creazione\u2026"
lastmod: '2024-03-13T22:44:42.938975-06:00'
model: gpt-4-0125-preview
summary: "L'interpolazione di stringhe in Google Apps Script consente l'incorporamento\
  \ dinamico di espressioni all'interno delle stringhe, facilitando la creazione di\
  \ codice pi\xF9 leggibile e manutenibile."
title: Interpolazione di una stringa
weight: 8
---

## Come fare:
In Google Apps Script, l'interpolazione di stringhe si ottiene attraverso i letterali template. Questi sono letterali di stringa che consentono espressioni incorporate, denotati da backticks (\`) invece delle solite virgolette. Ecco come puoi utilizzarli:

```javascript
// Un esempio base
function esempioDiInterpolazioneBase() {
  const utente = 'Alice';
  console.log(`Ciao, ${utente}!`); // Output: Ciao, Alice!
}

// Usando espressioni
function esempioDiInterpolazioneConEspressioni() {
  const a = 5;
  const b = 10;
  console.log(`Cinque più dieci fa ${a + b}.`); // Output: Cinque più dieci fa 15.
}

// Stringhe su più linee
function esempioDiStringaMultiLinea() {
  const elemento = 'Google Apps Script';
  console.log(`Questa è una stringa su più linee:
Ciao a tutti,
Oggi stiamo discutendo di ${elemento}.`);
  // Output:
  // Questa è una stringa su più linee:
  // Ciao a tutti,
  // Oggi stiamo discutendo di Google Apps Script.
}

esempioDiInterpolazioneBase();
esempioDiInterpolazioneConEspressioni();
esempioDiStringaMultiLinea();
```

Questi esempi illustrano l'uso di base, l'incorporamento di espressioni e la creazione di stringhe su più linee con valori interpolati.

## Approfondimento
I letterali template, compresa l'interpolazione di stringhe, sono stati introdotti in ECMAScript 2015 (ES6) e successivamente adottati in Google Apps Script. Prima di questo, i programmatori dovevano affidarsi puramente alla concatenazione di stringhe, che poteva diventare ingombrante per stringhe complesse o quando si integravano molti valori di variabili.

```javascript
// Vecchio metodo (prima di ES6)
var utente = 'Bob';
console.log('Ciao, ' + utente + '!');
```

Sebbene l'interpolazione di stringhe sia una funzionalità potente, è importante essere consapevoli dei contesti in cui viene utilizzata. Ad esempio, l'incorporamento diretto dell'input dell'utente senza una corretta sanificazione può portare a problemi di sicurezza, come attacchi di iniezione. Gli sviluppatori di Google Apps Script dovrebbero assicurarsi che qualsiasi contenuto dinamico interpolato nelle stringhe sia correttamente controllato o sanificato.

In confronto ad altri linguaggi di programmazione, il concetto di interpolazione di stringhe esiste ampiamente, con una sintassi variabile. Python utilizza le f-string o il metodo `format`, Ruby usa `#{}` all'interno di stringhe tra virgolette doppie, e molti linguaggi moderni hanno adottato caratteristiche simili a causa della leggibilità e della convenienza che offrono.

Sebbene Google Apps Script non offra funzionalità di interpolazione aggiuntive oltre a quelle fornite dagli standard ECMAScript, la funzionalità presente è potente e sufficiente per la maggior parte dei casi d'uso. Gli sviluppatori provenienti da linguaggi con meccanismi di interpolazione più elaborati potrebbero dover aggiustare le loro aspettative, ma apprezzeranno probabilmente la semplicità e l'efficienza dei letterali template in Google Apps Script.
