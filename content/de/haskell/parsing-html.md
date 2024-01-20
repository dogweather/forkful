---
title:                "HTML parsen"
date:                  2024-01-20T15:31:58.563713-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

Parsing von HTML bedeutet, den Quelltext einer Webseite in eine strukturierte Form umzuwandeln, die programmatisch verarbeitet werden kann. Das machen Programmierer, um Informationen zu extrahieren oder Webinhalte zu interagieren.

## Vorgehensweise:

In Haskell nutzen wir Bibliotheken wie `hxt` (Haskell XML Toolbox), um die Arbeit zu erleichtern. Hier ist ein einfaches Beispiel:

```Haskell
import Text.XML.HXT.Core

main :: IO ()
main = do
  let html = "<html><body><p>Hallo, Welt!</p></body></html>"
  runX $ readString [withParseHTML yes, withWarnings no] html
         >>> deep (isElem >>> hasName "p")
         >>> getText
```

Wenn du das ausführst, erhältst du als Ausgabe:

```
Hallo, Welt!
```

## Tiefgang:

Parsing von HTML kann trickreich sein, weil HTML oft nicht wohlgeformt ist. Historisch betrachtet war das Parsen daher fehleranfällig. Libraries wie `hxt` abstrahieren viele Herausforderungen. Alternativen in anderen Sprachen, wie Beautiful Soup für Python, zeigen, wie verbreitet das Bedürfnis nach robusten Parsing-Werkzeugen ist. Implementation details in `hxt` nutzen Lazy Parsing, Arrow-Notation für Klarheit und Anpassungsfähigkeit, und Fehlerbehandlung, um mit der Unbeständigkeit realer HTML-Dokumente umzugehen.

## Siehe auch:

- Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- Über XML Arrows in Haskell: https://en.wikibooks.org/wiki/Haskell/XML
- CSS Selektoren fürs Parsing mit Haskell: http://hackage.haskell.org/package/hxt-css-selectors