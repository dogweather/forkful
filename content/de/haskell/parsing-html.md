---
title:                "Haskell: HTML analysieren"
simple_title:         "HTML analysieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

HTML ist eine der grundlegenden Technologien des World Wide Web und wird für die Darstellung von Webseiten verwendet. Wenn Sie in der Programmierung tätig sind und Webdaten analysieren müssen, ist es daher hilfreich zu wissen, wie man HTML-Dokumente parsen kann. Dies ermöglicht es Ihnen, die Daten in einer strukturierten Form zu erhalten, die weiterverarbeitet und analysiert werden kann.

## Wie

Um HTML zu parsen, können Sie die Haskell-Bibliothek "tagsoup" verwenden. Importieren Sie zuerst die Bibliothek in Ihrem Code:

```Haskell
import Text.HTML.TagSoup
```

Als nächstes lesen Sie das HTML-Dokument ein und wandeln es in eine Liste von Tags um:

```Haskell
html <- readFile "index.html"
let tags = parseTags html
```

Sie können dann die Tags nach passenden Mustern durchsuchen und die gewünschten Daten extrahieren. Zum Beispiel, wenn Sie den Titel einer Webseite erhalten möchten, können Sie nach dem Tag "title" suchen und den Text innerhalb des Tags extrahieren:

```Haskell
let title = fromTagText $ head $ dropWhile (~/= TagOpen "title" []) tags
```

Das Ergebnis wird in einer einfachen Textform zurückgegeben, die Sie weiterverarbeiten können.

## Deep Dive

Das Parsen von HTML ist aufgrund der flexiblen und unstrukturierten Natur des Formats nicht immer einfach. Es gibt jedoch einige Tipps und Tricks, die Ihnen dabei helfen können:

- Seien Sie so spezifisch wie möglich mit Ihrem Musterabgleich. Verwenden Sie zum Beispiel nicht nur "div", um alle Div-Tags zu finden, sondern unterscheiden Sie zwischen Klassen und IDs, um nur die gewünschten Tags zu erhalten.
- Vermeiden Sie harte Codierung von Tag-Positionen. Versuchen Sie stattdessen, Tags nach ihrer Hierarchie und ihren Attributen zu suchen, um das Risiko von Fehlern zu minimieren.
- Beachten Sie die Verwendung von geschachtelten Tags und beachten Sie, dass Tags auch Attribute haben können.
- Nutzen Sie Haskell-Funktionen wie "map" und "filter", um die verarbeiteten Daten weiter zu transformieren.

See Also

- [Offizielle Dokumentation von Tagsoup](http://hackage.haskell.org/package/tagsoup)
- [Tutorial zu HTML-Parsing mit Tagsoup](https://www.codementor.io/@ayushdev/html-parsing-in-haskell-using-tagsoup-fhlddfq16)
- [Weitere nützliche Haskell-Bibliotheken](https://wiki.haskell.org/Applications_and_libraries/Web_programming)