---
date: 2024-01-20 17:47:30.376083-07:00
description: 'So geht''s: Dieser Code liefert die Anzahl der Zeichen im String `gruss`,
  inklusive Leerzeichen.'
lastmod: '2024-04-05T21:53:56.145397-06:00'
model: gpt-4-1106-preview
summary: Dieser Code liefert die Anzahl der Zeichen im String `gruss`, inklusive Leerzeichen.
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## So geht's:
```javascript
let gruss = "Hallo Welt!";
let laenge = gruss.length;
console.log(laenge); // Ausgabe: 11
```

Dieser Code liefert die Anzahl der Zeichen im String `gruss`, inklusive Leerzeichen.

```javascript
let leererString = "";
console.log(leererString.length); // Ausgabe: 0
```

Ein leerer String enthält 0 Zeichen.

## Tiefgang
Die `length` Eigenschaft eines Strings ist in JavaScript sehr direkt und einfach zu verwenden. Historisch gesehen ist die Einfachheit der String-Manipulation einer der Gründe, warum JavaScript so schnell zu einer dominanten Sprache des Webs wurde. Alternative Methoden, die Länge zu ermitteln, wie Schleifen zu benutzen, sind unnötig kompliziert und ineffizient verglichen mit `length`.

Im Detail: `length` gibt die Anzahl der UTF-16 Code-Einheiten zurück, die für den String verwendet werden. Dies kann wichtig sein, weil manche Zeichen, wie bestimmte Emoji, als zwei Code-Einheiten repräsentiert werden. Das bedeutet, dass `length` bei solchen Zeichen nicht immer der Anzahl der wahrgenommenen Zeichen entspricht.

## Weiterführende Quellen
- MDN Web Docs über String.length: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/length
- JavaScript Zeichenketten und Unicode: https://mathiasbynens.be/notes/javascript-unicode
