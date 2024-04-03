---
date: 2024-01-20 17:39:21.593123-07:00
description: 'How to: In TypeScript den `toLowerCase()` Methodenaufruf an einem String
  anzuwenden, konvertiert den gesamten Text in Kleinbuchstaben. So geht''s.'
lastmod: '2024-03-13T22:44:53.618885-06:00'
model: gpt-4-1106-preview
summary: In TypeScript den `toLowerCase()` Methodenaufruf an einem String anzuwenden,
  konvertiert den gesamten Text in Kleinbuchstaben.
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## How to:
In TypeScript den `toLowerCase()` Methodenaufruf an einem String anzuwenden, konvertiert den gesamten Text in Kleinbuchstaben. So geht's:

```typescript
let greeting: string = "Hallo Welt!";
let lowerCaseGreeting: string = greeting.toLowerCase();
console.log(lowerCaseGreeting); // "hallo welt!"
```

Einfach, oder? Funktioniert mit jedem String.

## Deep Dive
Historisch gesehen ist die Umwandlung von Texten in eine einheitliche Schreibweise eine alte Praxis, besonders in der Datenverarbeitung, um Inkonsistenzen zu vermeiden. In TypeScript und JavaScript wird dies durch die eingebaute `toLowerCase()` Funktion erledigt.

Alternativen? Man könnte eigene Funktionen schreiben, die das gleiche ohne `toLowerCase()` machen, aber warum das Rad neu erfinden?

Die Implementierung von `toLowerCase()` berücksichtigt auch die Komplikationen unterschiedlicher Sprachen und Zeichensätze. Zum Beispiel, das deutsche "ß" bleibt als "ß", da es kein direktes Pendant in Kleinbuchstaben gibt.

## See Also
Weitere Infos zu `String.prototype.toLowerCase()` findet man in der Mozilla Developer Network (MDN) Dokumentation:
- [MDN toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)

TypeScript-Dokumentation für Typisierung und best practices:
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)

Für besondere String-Manipulationen kann man auch auf Utilities von Lodash zurückgreifen:
- [Lodash String Methods](https://lodash.com/docs/#lowerCase)
