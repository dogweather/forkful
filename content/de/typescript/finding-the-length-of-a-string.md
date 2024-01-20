---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Länge eines Strings zu ermitteln ist im Grunde das Zählen der Zeichen in einem Strings. Programmierer machen das um Raum zu verwalten, Zeichen zu verarbeiten, oder um auf bestimmte Teile eines Strings zuzugreifen.

## So geht's:

Definieren Sie einen String und verwenden dann die `.length` Eigenschaft, um die Anzahl von Zeichen zu erhalten. Ganz einfach.

```TypeScript
let meinString: string = "Hallo Welt!";
console.log(meinString.length);

// Ausgabe: 12
```

## Tief Tauchen:

Historisch betrachtet ist die Methode zur Ermittlung der Länge eines Strings eine von den Anfangszeiten der Programmierung. Das Zählen von Zeichen in einem String ist seitdem ein integraler Bestandteil vieler Algorithmen und Funktionen.

Alternativen für die `.length` Eigenschaft gibt es in TypeScript eigentlich nicht. Jedoch könnte man natürlich manuell durch den String iterieren und einen Zähler hochzählen.

Die `.length` Eigenschaft in TypeScript (und auch in JavaScript) zählt Zeichen-Einheiten, nicht tatsächliche Zeichen. Das bedeutet, dass UniCode Zeichen, die aus zwei Zeichen-Einheiten bestehen, als zwei anstatt einem gezählt werden.

```TypeScript
let unicodeString: string = "🤓";
console.log(unicodeString.length);

// Ausgabe: 2
```

## Siehe Auch:

Jetzt, da Sie wissen, wie man die Länge eines Strings ermittelt, können Sie weitere Fähigkeiten erlernen. Hier sind einige nützliche Links:

- [MDN Web Docs: String.prototype.length](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [TypeScript: Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)