---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:10.631344-07:00
description: "Regul\xE4re Ausdr\xFCcke (regex) in JavaScript sind Muster, die verwendet\
  \ werden, um Zeichenkombinationen in Zeichenketten abzugleichen. Programmierer nutzen\u2026"
lastmod: '2024-03-13T22:44:54.256190-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke (regex) in JavaScript sind Muster, die verwendet\
  \ werden, um Zeichenkombinationen in Zeichenketten abzugleichen."
title: "Regul\xE4re Ausdr\xFCcke verwenden"
weight: 11
---

## Wie geht das:


### Einfache Übereinstimmung
Zum Start können Sie ein einfaches Regex-Muster erstellen und verwenden, um Übereinstimmungen in einem String zu finden. Hier werden wir das Wort "code" finden:

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### Verwendung von `String.prototype.match()`
Um ein Array von Übereinstimmungen zu erhalten:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### Globale Suche
Um alle Übereinstimmungen zu finden, nutzen Sie das `g` Flag:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Groß- und Kleinschreibung ignorierte Übereinstimmung
Das `i` Flag ignoriert die Groß- und Kleinschreibung:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Text ersetzen
Verwenden Sie `String.prototype.replace()`, um Teile des Strings zu ersetzen:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### Verwendung von Gruppen
Gruppen können Teile des Musters erfassen:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### Drittanbieter-Bibliotheken
Obwohl die integrierten Regex-Fähigkeiten von JavaScript leistungsfähig sind, könnten einige Aufgaben mit Bibliotheken wie `XRegExp` vereinfacht werden. Es bietet zusätzliche Syntax und Flags, die komplexe Muster lesbarer machen:

```javascript
// Beispiel für die XRegExp-Bibliothek
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

Dieser Schnipsel demonstriert die Verwendung von `XRegExp`, um alle Unicode-Wörter in einem String abzugleichen, und zeigt die Fähigkeit der Bibliothek, erweiterte Zeichensätze über die integrierten Fähigkeiten von JavaScript hinaus zu handhaben.
