---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:39.831106-07:00
description: "Le espressioni regolari (regex) sono pattern utilizzati per abbinare\
  \ combinazioni di caratteri in stringhe. I programmatori le utilizzano per cercare,\u2026"
lastmod: '2024-03-13T22:44:42.943799-06:00'
model: gpt-4-0125-preview
summary: "Le espressioni regolari (regex) sono pattern utilizzati per abbinare combinazioni\
  \ di caratteri in stringhe. I programmatori le utilizzano per cercare,\u2026"
title: Utilizzo di espressioni regolari
---

{{< edit_this_page >}}

## Cosa & Perché?

Le espressioni regolari (regex) sono pattern utilizzati per abbinare combinazioni di caratteri in stringhe. I programmatori le utilizzano per cercare, modificare o manipolare testo e dati, rendendole indispensabili per compiti di corrispondenza di modelli e analisi dei dati.

## Come fare:

Utilizzare espressioni regolari in Google Apps Script è semplice grazie alla sintassi basata su JavaScript. Ecco come puoi incorporare regex nei tuoi script per compiti comuni come la ricerca e la convalida dei dati.

### Ricerca nelle Stringhe

Supponi di voler trovare se una stringa contiene un pattern specifico, come un indirizzo email. Ecco un semplice esempio:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Trovato: " + found[0]);
  } else {
    Logger.log("Nessuna email trovata.");
  }
}

// Uso di esempio
findEmailInText("Contattaci su info@example.com.");
```

### Convalida dei Dati

Le espressioni regolari eccellono nella convalida dei dati. Di seguito una funzione che convalida una stringa di input per verificare se aderisce a una semplice politica per le password (almeno una lettera maiuscola, una lettera minuscola e un minimo di 8 caratteri).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Output di esempio
Logger.log(validatePassword("Str0ngPass")); // Stampa: true
Logger.log(validatePassword("debole"));     // Stampa: false
```

## Approfondimento

Le espressioni regolari in Google Apps Script sono ereditate da JavaScript, standardizzato per la prima volta nella specifica del linguaggio ECMAScript nel giugno del 1997. Sebbene potenti, a volte possono portare a codice confuso e difficile da mantenere, specialmente quando usate eccessivamente o per compiti di corrispondenza di modelli complessi che potrebbero essere risolti in modo più efficiente attraverso altri metodi di analisi.

Ad esempio, mentre puoi utilizzare le regex per l'analisi di HTML o XML in situazioni estreme, farlo è generalmente sconsigliato a causa delle strutture nidificate e complesse di questi documenti. Invece, strumenti specificamente progettati per l'analisi di tali strutture, come i parser DOM per HTML, sono più affidabili e leggibili.

Inoltre, gli sviluppatori di Google Apps Script dovrebbero essere consapevoli dei potenziali problemi di prestazione quando utilizzano pattern regex complessi in compiti di manipolazione di testo su larga scala, poiché l'elaborazione delle regex può essere intensiva per la CPU. In tali casi, suddividere il compito in sottocompiti più semplici o utilizzare funzioni di manipolazione delle stringhe incorporate potrebbe offrire un migliore equilibrio tra prestazioni e manutenibilità.
