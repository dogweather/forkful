---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Javascript: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug in der JavaSccript-Programmierung, das es ermöglicht, komplexe Such- und Ersetzungsmuster in Strings zu definieren. Sie können dabei helfen, effizient und präzise Daten zu analysieren, zu filtern und zu manipulieren. Durch die Verwendung von regulären Ausdrücken können Programmierer*innen ihre Codes kompakter, flexibler und lesbarer gestalten.

## Wie man reguläre Ausdrücke in JavaScript verwendet

Um reguläre Ausdrücke in JavaScript zu verwenden, muss man sie zunächst durch den RegEx-Konstruktor erstellen oder eine RegEx-Literal-Notation verwenden. Nehmen wir an, wir möchten alle Vokale in einem String zählen. Mit RegEx kann man Folgendes tun:

```Javascript
let string = "Hallo Welt";
let regex = /[aeiou]/g;
let matches = string.match(regex);
console.log(matches);
```

Dies würde die Ausgabe `[a, e, e]` liefern. Wie man sehen kann, wird durch die Verwendung des RegEx-Konstruktors mit der `g` Flagge alle Vokale in dem gegebenen String gefunden. 

Man kann auch reguläre Ausdrücke für Ersetzungen verwenden. Angenommen, wir möchten alle Zahlen in einem String mit `x` ersetzen. Mit RegEx wäre das so möglich:

```Javascript
let string = "Die Antwort lautet 42";
let regex = /\d+/g;
let newString = string.replace(regex, 'x');
console.log(newString);
```

Die Ausgabe würde dann `Die Antwort lautet x` lauten. In diesem Fall ersetzt die Methode `replace` alle Zahlen in dem String mit dem gegebenen Wert. 

## Tiefes Eintauchen in reguläre Ausdrücke

Bei der Verwendung von regulären Ausdrücken gibt es verschiedene Metazeichen, die bestimmte Zeichen oder Zeichenfolgen repräsentieren. Im vorherigen Beispiel haben wir beispielsweise die Metazeichen `[` und `]` verwendet, um die Menge der Vokale anzugeben. Hier sind einige andere nützliche Metazeichen:

- `.` repräsentiert jedes einzelne Zeichen
- `\d` repräsentiert eine beliebige Zahl
- `\w` repräsentiert einen Buchstaben oder eine Zahl
- `\s` repräsentiert ein Leerzeichen oder Tabulator
- `*` repräsentiert eine beliebige Anzahl von Wiederholungen des vorherigen Charakters
- `+` repräsentiert eine oder mehrere Wiederholungen des vorherigen Charakters
- `?` repräsentiert eine oder keine Wiederholungen des vorherigen Charakters
- `^` repräsentiert den Anfang einer Zeile
- `$` repräsentiert das Ende einer Zeile

Für eine ausführliche Liste der Metazeichen und deren Funktionen, schauen Sie sich gerne die folgenden Links an.

## Siehe auch

- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExp in JavaScript einfach erklärt](https://www.einfachproggen.de/regulare-ausdrucke-regexp-in-javascript-einfach-erklart/)
- [Reguläre Ausdrücke Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/javascript)