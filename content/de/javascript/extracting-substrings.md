---
title:                "Javascript: Extrahieren von Teilstrings"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum?

Das Extrahieren von Teilstrings ist eine wichtige Fähigkeit in der JavaScript-Programmierung. Es ermöglicht es uns, spezifische Teile eines Textes zu isolieren und zu bearbeiten. Diese Fähigkeit ist besonders nützlich bei der Verarbeitung von Benutzereingaben oder beim Manipulieren von großen Datenmengen.

## Wie Geht's?

Um Teilstrings in JavaScript zu extrahieren, verwenden wir die `substring()` Methode. Diese Methode akzeptiert zwei Parameter: den Startindex und den Endindex des gewünschten Teilstrings. Hier ist ein Beispiel, wo wir den Teilstring "welt" aus dem Satz "Hallo Welt!" extrahieren:

```Javascript
var satz = "Hallo Welt!";
var teilstring = satz.substring(6, 10);
console.log(teilstring); // Output: welt
```

In diesem Beispiel haben wir den Startindex auf 6 und den Endindex auf 10 gesetzt, da der Teilstring "welt" fünf Zeichen hat und der Index in JavaScript bei 0 beginnt.

Um den Endindex weglassen und den Teilstring bis zum Ende des Satzes extrahieren zu können, können wir stattdessen nur den Startindex angeben:

```Javascript
var teilstring = satz.substring(6);
console.log(teilstring); // Output: Welt!
```

Dieses Mal wird der Teilstring ab dem 6. Zeichen bis zum Ende des Satzes extrahiert.

## Tiefere Einblicke

Die `substring()` Methode ist eine von mehreren Methoden, die es uns ermöglicht, Teilstrings in JavaScript zu extrahieren. Andere Methoden wie `slice()` und `substr()` bieten ähnliche Funktionalitäten, haben aber einige Unterschiede in der Syntax und den unterstützten Parametern.

Eine wichtige Sache, die man beim Extrahieren von Teilstrings beachten sollte, ist die Behandlung von Unicode-Zeichen. Während die `substring()` und `slice()` Methoden die Zeichen nach ihrer Position behandeln, kann die `substr()` Methode auch "surrogate pairs" erkennen, die aus zwei Code Points bestehen und ein Zeichen repräsentieren.

Es ist wichtig, die Unterschiede zwischen diesen Methoden zu kennen, um die geeignetste für unsere spezifischen Anforderungen zu wählen. Eine ausführliche Erläuterung dieser Unterschiede findet man in der offiziellen JavaScript-Dokumentation.

## Siehe Auch

Für weitere Informationen über das Extrahieren von Teilstrings in JavaScript, schauen Sie sich diese Ressourcen an:

- [JavaScript-String-Methoden](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Die Unterschiede zwischen substring(), slice() und substr()](https://www.freecodecamp.org/learn/javascript-algorithms-and-data-structures/basic-javascript/the-difference-between-the-splice-and-slice-methods)
- [Wie man Unicode-Zeichen richtig behandelt](https://medium.com/better-programming/how-to-make-sure-your-javascript-app-works-with-non-english-unicode-characters-e158b8143b47)