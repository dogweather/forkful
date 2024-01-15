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

## Warum

Wenn du softwareengineering, Webentwicklung oder Datenanalyse betreibst, wirst du früher oder später mit der Aufgabe konfrontiert werden, Text zu suchen und zu ersetzen. Dies kann z.B. nützlich sein, um Fehler zu korrigieren oder Daten schnell zu bereinigen.

## Wie man Text in Javascript sucht und ersetzt

Um Text in Javascript zu suchen und zu ersetzen, gibt es verschiedene Methoden und Techniken. Hier sind einige Beispiele, wie du dies in deinem Code umsetzen kannst:

1) Die `replace()`-Methode: Diese Methode kann verwendet werden, um einen bestimmten Teil eines Strings durch einen anderen zu ersetzen. Hier ist ein Beispiel, wie man alle Vorkommen von "Hallo" in einem String durch "Guten Tag" ersetzt:

```Javascript
let text = "Hallo, mein Name ist Max. Hallo an alle Leser.";
let newText = text.replace("Hallo", "Guten Tag");
// output: "Guten Tag, mein Name ist Max. Guten Tag an alle Leser."
```

2) Die `replaceAll()`-Methode (ab ECMAScript 2021): Diese Methode funktioniert ähnlich wie `replace()`, jedoch werden alle Vorkommen des gesuchten Textes ersetzt. Hier ist das obige Beispiel mit `replaceAll()` umgesetzt:

```Javascript
let text = "Hallo, mein Name ist Max. Hallo an alle Leser.";
let newText = text.replaceAll("Hallo", "Guten Tag");
// output: "Guten Tag, mein Name ist Max. Guten Tag an alle Leser."
```

3) Reguläre Ausdrücke: Reguläre Ausdrücke (auch bekannt als "RegExp") sind eine sehr leistungsstarke Möglichkeit, Text in Javascript zu suchen und zu ersetzen. Sie erlauben es, nach bestimmten Mustern zu suchen und diese zu ersetzen. Hier ist ein Beispiel, wie man alle Vorkommen von "Hallo" oder "Guten Tag" in einem String durch "Hello" ersetzt:

```Javascript
let text = "Hallo, mein Name ist Max. Guten Tag an alle Leser.";
let newText = text.replace(/(Hallo|Guten Tag)/g, "Hello");
// output: "Hello, mein Name ist Max. Hello an alle Leser."
```

## Tiefergehende Informationen

Die oben genannten Beispiele sind nur ein kleiner Ausschnitt aus den Möglichkeiten, die Javascript bietet, um Text zu suchen und zu ersetzen. Hier sind weitere hilfreiche Ressourcen, die dir dabei helfen können, dieses Thema noch besser zu verstehen:

- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: String.prototype.replaceAll()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll)
- [MDN Web Docs: RegExp](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/RegExp)

## Siehe auch

- [How to Use Regular Expressions (Regex) in Javascript](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-regex-in-javascript)
- [10 RegEx-Beispiele zum Lernen und Üben](https://www.freecodecamp.org/news/regex-cheat-sheet/)
- [Guide to Regular Expressions in Javascript](https://flaviocopes.com/javascript-regular-expressions/)