---
title:                "Entfernen von Zeichen, die einem Muster entsprechen"
html_title:           "Javascript: Entfernen von Zeichen, die einem Muster entsprechen"
simple_title:         "Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Was und Warum?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine Funktion in der Javascript-Programmierung, die es ermöglicht, bestimmte Zeichen in einer Zeichenfolge zu entfernen. Diese Funktion wird von Programmierern häufig genutzt, um unerwünschte Zeichen in einer Eingabe zu entfernen oder um bestimmte Filter anzuwenden.

# Wie geht's?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in Javascript auf verschiedene Weise durchgeführt werden. Eine einfache Methode ist die Verwendung der ```replace()``` Funktion mit Hilfe von regulären Ausdrücken. Ein Beispiel dafür ist:

```Javascript
let input = "H3ll0 W0rld!";
let output = input.replace(/[0-9]/g, "");
console.log(output); // Gibt "Hll Wrld!" aus
```

Hier wird der reguläre Ausdruck ```/[0-9]/g``` verwendet, um alle Zahlen in der Zeichenfolge zu finden und sie durch einen leeren String zu ersetzen, was sie effektiv löscht.

Eine weitere Möglichkeit besteht darin, die ```split()``` Funktion zu verwenden und dann die gewünschten Zeichen aus dem resultierenden Array zu entfernen. Ein Beispiel dafür wäre:

```Javascript
let input = "Hello World!";
let output = input.split("o").join("");
console.log(output); // Gibt "Hell Wrld!" aus
```

In diesem Beispiel wird die ```split()``` Funktion verwendet, um die Zeichenfolge an jedem Vorkommen des Buchstabens "o" zu teilen. Dann wird die ```join()``` Funktion verwendet, um das Array wieder in eine Zeichenfolge umzuwandeln, wobei das gewünschte Zeichen entfernt wird.

# Einblick

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine häufig verwendete Funktion in der Programmierung, die jedoch meist in Kombination mit anderen Funktionen eingesetzt wird, um bestimmte Aufgaben zu erledigen. Zum Beispiel kann die Funktion verwendet werden, um Benutzereingaben zu validieren oder um Daten zu normalisieren.

Alternativ können Programmierer auch die ```slice()``` oder ```substring()``` Funktionen verwenden, um bestimmte Zeichen aus einer Zeichenfolge zu entfernen. Diese Funktionen bieten mehr Flexibilität in Bezug auf die Auswahl der zu entfernenden Zeichen. Eine andere Möglichkeit ist die Verwendung von Bibliotheken wie ```lodash``` oder ```underscore```, die spezielle Funktionen zum Bearbeiten von Zeichenfolgen anbieten.

Bei der Implementation des Löschens von Zeichen, die einem bestimmten Muster entsprechen, gibt es einige wichtige Dinge zu beachten. Zum Beispiel ist es wichtig, den richtigen regulären Ausdruck zu verwenden, um das gewünschte Zeichenmuster genau zu treffen. Außerdem sollte darauf geachtet werden, dass die ursprüngliche Zeichenfolge nicht versehentlich verändert wird, wenn sie von einer anderen Funktion zurückgegeben wird.

# Siehe Auch

- [Regular Expressions in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [String.prototype.split()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)