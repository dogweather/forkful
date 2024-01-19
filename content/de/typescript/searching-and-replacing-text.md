---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen ist eine leistungsfähige Funktion, die Suchmuster erkennt und den entsprechenden Text durch einen Ersatzwert ersetzt. Programmierer nutzen sie, um Code zu optimieren, Fehler zu beheben oder Informationen in Datenstrukturen zu aktualisieren.

## Wie macht man das:

Ein einfacher Weg, Text zu suchen und zu ersetzen, ist die `replace()` Methode in TypeScript. Hier ist ein Beispiel:

```TypeScript
let text = 'Hallo Welt, wie geht es dir Welt?';
let suchmuster = /Welt/g;
let ersatz = 'Erde';

let ergebnisText = text.replace(suchmuster, ersatz);

console.log(ergebnisText); // "Hallo Erde, wie geht es dir Erde?"
```

In diesem Beispiel suchen wir nach allen Vorkommen von "Welt" (dank dem /g Faktor) und ersetzen sie durch "Erde".

## Deep Dive

Suchen und Ersetzen von Text gibt es seit den frühesten Tagen der Programmiersprachen. Sie wurde hauptsächlich verwendet, um Antworten in Mensch-Maschine Kommunikation zu schreiben oder Daten in Datenbanken zu aktualisieren.

Alternativen zu `replace()` sind Methoden wie `substr()` oder `split()` zum Finden, und dann `concat()` oder `join()` zum Neuverbinden von Strings.

Die genaue Implementierung von `replace()` variiert je nach Sprache und Engine. In der Regel nutzen es ähnliche Konzepte: Reguläre Ausdrücke zur Identifizierung des Suchmusters und String-Manipulation zur Durchführung der Ersatzoperation.

## Siehe Auch:

- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [W3Schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)