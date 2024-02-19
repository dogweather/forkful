---
aliases:
- /de/javascript/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:42:37.115483-07:00
description: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, ist ein Filterprozess:\
  \ Unn\xF6tiges oder st\xF6rendes Zeugs fliegt raus. Programmierer nutzen das, um\u2026"
lastmod: 2024-02-18 23:09:05.262100
model: gpt-4-1106-preview
summary: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, ist ein Filterprozess:\
  \ Unn\xF6tiges oder st\xF6rendes Zeugs fliegt raus. Programmierer nutzen das, um\u2026"
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Löschen von Zeichen, die einem Muster entsprechen, ist ein Filterprozess: Unnötiges oder störendes Zeugs fliegt raus. Programmierer nutzen das, um Daten zu bereinigen, zu validieren oder zu verarbeiten. Einfache Beispiele sind das Entfernen ungültiger Zeichen in einer Eingabe oder das Bereinigen von Textdateien.

## How to: (Wie geht das?)
In JavaScript benutzt man häufig Regular Expressions (Regex), um Muster in Texten zu identifizieren und zu löschen. Hier ein schnelles Beispiel:

```javascript
const string = "B3r1in ist WU5NDER5chön!";
const pattern = /[0-9]/g; // Muster das Zahlen sucht
const cleanedString = string.replace(pattern, '');

console.log(cleanedString); // "Berlin ist WUNDERSchön!"
```

Einfach, oder? `replace()` sucht hier Zeichen, die dem Muster entsprechen, und ersetzt sie durch nichts (also löscht sie).

## Deep Dive (Tiefer eintauchen)
The 'replace' Funktion in JavaScript ist seit ihren Anfängen dabei. Im Kern nutzt sie Regex, um Muster zu definieren. Regex kann simpel oder absurd komplex sein, je nach Bedarf.

Alternativen zum Löschen von Zeichen gibt es: Man könnte durch den String iterieren und jeden Buchstaben checken. Das ist aber oft langsamer und mühsamer im Code.

Implementation: Die moderne JavaScript Engine verwendet effiziente Algorithmen, um 'replace' Operationen zu beschleunigen. Die `/g`-Flagge in Regex steht für 'global' und veranlasst die Suche im gesamten String.

## See Also (Siehe auch)
Um die Regex-Künste zu schärfen, hier ein paar Ressourcen:

- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) – Grundlagen und Beispiele für Regex in JS.
- [Regex101](https://regex101.com/) – Ein Online-Tool zum Testen von Regex mit Erklärungen und Code-Generierung für verschiedene Programmiersprachen.
