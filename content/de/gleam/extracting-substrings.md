---
title:                "Unterstrings extrahieren"
html_title:           "Gleam: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Extrahieren von Teilzeichenketten bedeutet, Teile eines Textes oder einer Zeichenkette zu isolieren und zu verwenden. Programmierer nutzen dies, um bestimmte Informationen aus einer größeren Zeichenkette zu extrahieren oder zu verarbeiten.

Wie geht's?
Gleam bietet eine einfache Möglichkeit, Teilzeichenketten aus einer Zeichenkette zu extrahieren. Hier sind zwei Beispiele, um zu zeigen, wie es funktioniert:

```Gleam
let text = "Hallo, mein Name ist Max";
let result = String.split(text, ", ");
// result ist nun ["Hallo", "mein Name ist Max"]

let name = String.slice(text, 18, 21);
// name ist nun "Max"
```

Tiefen-Tauchen
Das Extrahieren von Teilzeichenketten gibt es schon seit langer Zeit und ist eine grundlegende Funktion in den meisten Programmiersprachen. Einige Alternativen zu Gleam sind die Verwendung von regulären Ausdrücken oder benutzerdefinierten Funktionen. In Gleam wird das Extrahieren von Teilzeichenketten durch die Verwendung von String-Methoden wie `String.split` und `String.slice` ermöglicht.

Siehe auch
- [Gleam-Dokumentation über Teilzeichenketten](https://gleam.run/getting-started/strings.html#substring)
- [Eine Einführung in das Extrahieren von Teilzeichenketten in Python](https://realpython.com/python-string-split-concatenate-join/)
- [Eine Übersicht über reguläre Ausdrücke](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)