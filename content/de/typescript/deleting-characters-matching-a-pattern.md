---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---

## Was & Warum?

Das Entfernen von Zeichen, die einem bestimmten Muster entsprechen, ist eine gängige Programmieroperation. Dies ist nützlich, um unerwünschte Zeichen aus Daten, wie bestimmte Sonderzeichen oder Leerzeichen, zu entfernen.

---

## So geht's:

Duplizieren wir zunächst einen String und löschen dann einige Zeichen daraus:

```TypeScript
let text: string = "Hallo Welt! Willkommen in der TypeScript Welt!";
let pattern: RegExp = /Welt/gi;
let result: string = text.replace(pattern, "");
console.log(result); // "Hallo ! Willkommen in der TypeScript !"
```

In diesem Fall haben wir das Muster "Welt" (unabhängig von Groß- und Kleinschreibung) gesucht und alle übereinstimmenden Zeichenfolgen durch nichts ersetzt, um sie effektiv zu löschen.

---

## Nachforschung:

Historisch gesehen entstand die Notwendigkeit, Zeichen nach einem Muster zu löschen, mit der Entwicklung von Textverarbeitungsprogrammen. Heute wird es weitreichend in vielen Bereichen der Softwareentwicklung eingesetzt.

Alternativ könnten wir eine Schleife verwenden, um durch jeden Charakter im String zu iterieren, dies wäre jedoch ineffizienter und anfällig für Fehler.

Die `RegExp`- und die `replace()`-Methode in TypeScript ist eine leistungsstarke und flexible Methode, um dieses Problem zu lösen. Sie ermöglicht es, komplexe Muster zu definieren und Gruppen von Zeichen in einem Schritt zu ersetzen.

---

## Siehe auch:

- [MDN Dokumentation zu RegExp](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [MDN Dokumentation zu String.prototype.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)