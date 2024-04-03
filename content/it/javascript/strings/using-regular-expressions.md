---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:12.320242-07:00
description: "Le espressioni regolari (regex) in JavaScript sono pattern utilizzati\
  \ per abbinare combinazioni di caratteri nelle stringhe. I programmatori le usano\
  \ per\u2026"
lastmod: '2024-03-13T22:44:43.801877-06:00'
model: gpt-4-0125-preview
summary: Le espressioni regolari (regex) in JavaScript sono pattern utilizzati per
  abbinare combinazioni di caratteri nelle stringhe.
title: Utilizzo delle espressioni regolari
weight: 11
---

## Come fare:


### Abbinamento Base
Per iniziare, puoi creare un semplice pattern regex e usarlo per trovare corrispondenze in una stringa. Qui, troveremo la parola "code":

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### Utilizzando `String.prototype.match()`
Per recuperare un array di corrispondenze:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### Ricerca Globale
Per trovare tutte le corrispondenze, usare il flag `g`:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Abbinamento Insensibile al Maiuscolo/Minuscolo
Il flag `i` ignora il maiuscolo/minuscolo:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Sostituire Testo
Usa `String.prototype.replace()` per sostituire parti della stringa:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### Utilizzando Gruppi
I gruppi possono catturare parti del pattern:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### Librerie di Terze Parti
Sebbene le capacità regex integrate in JavaScript siano potenti, alcune attività potrebbero essere semplificate con librerie come `XRegExp`. Offre una sintassi aggiuntiva e flag, rendendo i pattern complessi più leggibili:

```javascript
// Esempio della libreria XRegExp
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

Questo frammento dimostra l'uso di `XRegExp` per abbinare tutte le parole Unicode in una stringa, mostrando la capacità della libreria di gestire insiemi di caratteri estesi oltre le capacità integrate di JavaScript.
