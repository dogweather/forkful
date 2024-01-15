---
title:                "HTML parsen"
html_title:           "Gleam: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Parsing von HTML-Dokumenten kann für viele Programmiererinnen und Programmierer eine nützliche Fähigkeit sein, um Daten aus Webseiten zu extrahieren. Es ermöglicht auch das automatisierte Durchsuchen und Verarbeiten von großen Mengen an HTML-Code, was hilfreich sein kann, wenn man beispielsweise Daten für eine Datenbank oder eine Analyse benötigt.

## Wie geht man vor?

Um HTML mit Gleam zu parsen, verwenden wir das Paket "html-parser", das Code-Blöcke in einer HTML-Datei in Strukturen umwandelt, die in Gleam leichter zu verarbeiten sind. Hier ist ein einfaches Beispiel, wie man Gleam-Code schreibt, um eine HTML-Datei zu parsen:

```Gleam
import html-parser

// Den HTML-Code als String deklarieren
let html = "<html><body><h1>Hello, Gleam!</h1></body></html>"

// Den HTML-Code parsen und in eine Struktur umwandeln
let parsed = html_parser.parse(html)

// Die Struktur nach Elementen durchsuchen, in diesem Fall <h1>
let title = parsed
  .find_all
  .last()
  .filter(|e| e.tag == "h1")
  .unwrap()

// Den Inhalt des Elements ausgeben
io.print(title.content) // Ausgabe: Hello, Gleam!
```

Der Aufruf von `parse()` gibt ein Ergebnis vom Typ `Result(HtmlParserError, List(HtmlElement))` zurück. Wenn die HTML-Datei erfolgreich geparst wird, enthält die Liste alle gefundenen Elemente in der Reihenfolge, in der sie im Dokument auftreten. Mit den Methoden `find_all` und `filter` kann man bestimmte Elemente auswählen, die in einer Liste zurückgegeben werden.

## Tiefer Einblick

Das "html-parser" Paket bietet auch die Möglichkeit, bestimmte CSS-Selektoren zu verwenden, um gezielt Elemente auszuwählen. Diese Methode kann besonders nützlich sein, wenn man nur bestimmte Teile einer Webseite benötigt. Zum Beispiel kann man mit dem CSS-Selektor `h1#title` nur das erste `h1`-Element mit der ID "title" auswählen.

Es ist außerdem möglich, die Attribute eines Elements auszulesen, z.B. `class`, `id` oder benutzerdefinierte Attribute. So kann man beispielsweise alle Links auf einer Webseite auslesen, indem man nach dem `a`-Tag filtert und das `href`-Attribut ausgibt.

Zusätzlich bietet das Paket Funktionen zum Verschachteln von Elementen und zum Entfernen von Tags, die nützlich sein können, wenn man nur Text aus einer Webseite extrahieren möchte.

## Siehe auch

- Offizielle Dokumentation zum "html-parser" Paket: https://github.com/gleam-lang/html-parser
- Einführung in Gleam: https://gleam.run/getting-started/introduction
- Gleam Community-Forum: https://forum.gleam.run/

Wenn Sie mehr über das Parsen von HTML mit Gleam erfahren möchten, können Sie die offizielle Dokumentation des "html-parser" Pakets durchlesen oder sich in der Gleam Community umsehen. Nutzen Sie diese Fähigkeit, um Ihre Programmierprojekte noch effizienter zu gestalten!