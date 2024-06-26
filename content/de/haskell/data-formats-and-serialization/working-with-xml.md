---
date: 2024-01-26 04:31:50.800648-07:00
description: "Wie geht das? Haskell bietet Bibliotheken wie `xml-conduit` f\xFCr den\
  \ Umgang mit XML. Das folgende Beispiel demonstriert das Parsen eines XML-Strings\
  \ und\u2026"
lastmod: '2024-03-13T22:44:53.954826-06:00'
model: gpt-4-0125-preview
summary: "Haskell bietet Bibliotheken wie `xml-conduit` f\xFCr den Umgang mit XML."
title: Arbeiten mit XML
weight: 40
---

## Wie geht das?
Haskell bietet Bibliotheken wie `xml-conduit` für den Umgang mit XML. Das folgende Beispiel demonstriert das Parsen eines XML-Strings und das Abfragen von Elementen:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualifiziert Data.Text as T
import Text.XML
import Text.XML.Cursor

main :: IO ()
main = do
  let xmlContent = "<greetings><hello>World!</hello></greetings>"
  let document = parseLBS_ def $ T.encodeUtf8 $ T.pack xmlContent
  let cursor = fromDocument document

  let helloTexts = cursor $// element "hello" &/ content
  print helloTexts  -- ['World!']
```

Beispielausgabe:

```
["World!"]
```

## Tiefgehende Betrachtung
XML, kurz für Extensible Markup Language, ist schon lange vor dem Aufstieg von JSON ein Grundpfeiler in der Daten-Serialisierung gewesen. Es ist umständlich, aber starr und standardisiert, was es geeignet für strenge Unternehmensumgebungen, Altsysteme und Branchen wie Finanzen und Gesundheitswesen macht.

Haskell hat mehrere Bibliotheken für XML; jedoch ist `xml-conduit` aufgrund seiner effizienten Streaming- und Parsfähigkeiten, Teil der `conduit`-Familie für die Handhabung von Datenströmen, eine der leistungsfähigsten und am weitesten verbreiteten.

Alternativen beinhalten `HXT` (Haskell XML Toolbox), das Pfeile für das Parsen und die Transformation verwendet und somit ein anderes Paradigma für XML-Manipulationen bietet. Obwohl `HXT` aufgrund seiner steileren Lernkurve heutzutage weniger beliebt ist, bleibt es doch für einige Anwendungsfälle eine solide Wahl.

Bei der Implementierung der XML-Verarbeitung in Haskell müssen Sie sich um die Kodierung kümmern, da Haskell-Strings Unicode sind und XML-Daten dies möglicherweise nicht sind. Zusätzlich können XML-Namensräume die Parsearbeit erschweren.

## Siehe auch:
- Die `xml-conduit` Paketdokumentation: https://hackage.haskell.org/package/xml-conduit
- Die Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- Buch "Real World Haskell", Kapitel 16, zur XML-Handhabung: http://book.realworldhaskell.org/read/xml.html
- Haskell Wiki zu XML: https://wiki.haskell.org/XML
