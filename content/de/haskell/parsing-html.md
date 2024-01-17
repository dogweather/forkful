---
title:                "HTML aufteilen"
html_title:           "Haskell: HTML aufteilen"
simple_title:         "HTML aufteilen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen von HTML ist ein wichtiger Aspekt der Webentwicklung, der es Programmierern ermöglicht, Informationen aus HTML-Dokumenten zu extrahieren und zu manipulieren. Dies ist nützlich für das Web-Scraping, die Automatisierung von Aktionen auf einer Webseite oder die Konvertierung von HTML-Daten in eine andere Struktur. Es ist auch ein wichtiger Teil der Entwicklung von Web-Crawlern und Suchmaschinen.

## Wie geht's?

Das Parsen von HTML in Haskell kann mit verschiedenen Bibliotheken wie "HXT" oder "TagSoup" erreicht werden. Zum Beispiel:

```Haskell
import Text.XML.HXT.Core
main = do
  html <- readFile "index.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  nodes <- runX $ doc //> hasName "title"
  putStr $ show nodes 
  -- Ausgabe: [NTree (XTag "title" []) [NTree (XText "Meine Webseite") []]]
```

In diesem Beispiel wird mit Hilfe von "readString" ein HTML-Dokument in ein XML-Dokument umgewandelt, das dann von den mächtigen Selektionsmöglichkeiten von "runX" verwendet werden kann, um bestimmte Elemente wie den Titel der Seite zu finden.

## Tiefere Einblicke

Das Parsen von HTML hat eine lange Geschichte und es gibt viele verschiedene Ansätze und Bibliotheken dafür. In Haskell können auch Parser-Kombinatoren verwendet werden, um spezifische Parser für bestimmte Teile von HTML zu erstellen. Eine bekannte Bibliothek dafür ist "Taggy", die auch für CSS-Selektoren verwendet werden kann.

## Siehe auch

- [Offizielle Dokumentation für HXT](http://hackage.haskell.org/package/hxt) 
- [Offizielle Dokumentation für TagSoup](http://hackage.haskell.org/package/tagsoup) 
- [Github-Repository für Taggy](https://github.com/repair-man/taggy)