---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Länge eines Strings zu finden, bezieht sich auf die Bestimmung der Anzahl der Zeichen, die es enthält. Programmierer tun dies oft, um die Eingabevalidierung durchzuführen oder um die Stringmanipulationen zu kontrollieren.

## Wie zu:

```Javascript
let meinString = 'Hallo Welt';
console.log(meinString.length);
```

Die Ausgabe wäre `11`, weil `Hallo Welt` 11 Zeichen enthält (einschließlich des Leerzeichens).

Sie können sogar die Länge eines leeren Strings überprüfen:

```Javascript
let leererString = '';
console.log(leererString.length);
```

Die Ausgabe wird `0` sein, weil der String keine Zeichen enthält.

## Deep Dive

Die Methode `.length` in JavaScript stammt aus den frühen Tagen von Netscape (ja, das dürfte dich zurückbringen). Sie hat sich als wirksames und einfaches Mittel zur Erkennung der Stringlänge behauptet.

Alternativ könnten Sie eine Schleife verwenden, um jedes Zeichen einzeln zu zählen, aber `.length` ist in den meisten Fällen effizienter und einfacher zu verwenden.

Die Implementierung von `.length` in JavaScript ist ziemlich gerade. Es verfolgt einfach die Anzahl der Zeichen innerhalb des string literal und gibt diesen Wert zurück. Für die Sprache selbst sind keine zusätzlichen Berechnungen erforderlich.

## Siehe Auch

Für weitere Informationen zu Strings in JavaScript, besuchen Sie die MDN Web Docs: [String](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String)

JavaScript has a lot of methods to manipulate and work with strings, not just `.length`. Check them out here: [String.prototype](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/prototype)