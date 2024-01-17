---
title:                "Suchen und Ersetzen von Text"
html_title:           "Javascript: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Was & Warum?

Textsuche und -ersetzung ist eine häufige Aufgabe in der Programmierung. Es bezieht sich auf das Finden von bestimmten Textstücken in einem Programm oder einer Datei und deren Austausch mit einem anderen Textstück. Programmierer nutzen diese Funktion, um schnell und effektiv größere Änderungen im Code vorzunehmen oder Fehler zu korrigieren.

# Wie geht's?

Ein gutes Beispiel für die Textsuche und -ersetzung ist die Verwendung von regulären Ausdrücken in Javascript. Nehmen wir an, wir haben einen String, der das Wort "programmierer" enthält, aber wir möchten es durch "coder" ersetzen. Wir können das folgendermaßen tun:

```Javascript
let text = "Ich bin ein Programmierer.";

text = text.replace(/programmierer/i, 'coder');

console.log(text); // Ausgabe: Ich bin ein Coder.
```

Hier haben wir die `replace()` Methode verwendet, um das Wort "programmierer" global zu suchen und durch "coder" zu ersetzen. Das `i` in `/programmierer/i` steht für "ignore case", was bedeutet, dass die Suche unabhängig von Groß- und Kleinschreibung gewesen ist.

# Tiefentauchen

Kodieren geht zurück auf frühe Entwickler, die schwierige Aufgaben der Kodierung von Maschinencode automatisieren wollten, um Fehler zu vermeiden und die Effizienz zu steigern. Heutzutage gibt es auch andere Möglichkeiten, Textsuche und -ersetzungen durchzuführen, z.B. mit Texteditoren oder speziellen Tools. In Javascript bieten auch andere Methoden, wie z.B. `split()` und `join()`, verschiedene Optionen zur Manipulation von Text. Es ist wichtig zu verstehen, wie reguläre Ausdrücke funktionieren und wie sie in verschiedenen Sprachen und Tools eingesetzt werden können.

# Siehe auch

Hier sind einige nützliche Links, die Ihnen helfen können, mehr über die Textsuche und -ersetzung in Javascript zu erfahren:

- [MDN Web Docs - String replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [Learn Regex the Hard Way](https://regex.learncodethehardway.org/)