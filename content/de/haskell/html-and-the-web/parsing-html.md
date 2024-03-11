---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:38.570923-07:00
description: "HTML in Haskell zu parsen, erm\xF6glicht es Ihnen, Daten zu extrahieren,\
  \ HTML-Inhalte zu manipulieren oder programmgesteuert mit Webseiten zu interagieren.\u2026"
lastmod: '2024-03-11T00:14:27.826238-06:00'
model: gpt-4-0125-preview
summary: "HTML in Haskell zu parsen, erm\xF6glicht es Ihnen, Daten zu extrahieren,\
  \ HTML-Inhalte zu manipulieren oder programmgesteuert mit Webseiten zu interagieren.\u2026"
title: HTML parsen
---

{{< edit_this_page >}}

## Was & Warum?

HTML in Haskell zu parsen, ermöglicht es Ihnen, Daten zu extrahieren, HTML-Inhalte zu manipulieren oder programmgesteuert mit Webseiten zu interagieren. Diese Operation ist wesentlich für Aufgaben wie Web Scraping, automatisiertes Testen von Webanwendungen und das Minen von Daten von Websites - unter Ausnutzung von Haskells starkem Typsystem und funktionalen Programmierparadigmen, um robusten und prägnanten Code zu gewährleisten.

## Wie geht das:

Um HTML in Haskell zu parsen, verwenden wir die `tagsoup` Bibliothek aufgrund ihrer Einfachheit und Flexibilität. Stellen Sie zunächst sicher, dass Sie die Bibliothek installieren, indem Sie `tagsoup` zur cabal-Datei Ihres Projekts hinzufügen oder `cabal install tagsoup` ausführen.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- Beispiel HTML zur Demonstration
let sampleHtml = "<html><body><p>Lerne Haskell!</p><a href='http://example.com'>Hier klicken</a></body></html>"

-- HTML parsen und nach Links filtern (a-Tags)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- Extrahierte Links ausgeben
print links
```

Beispielausgabe:
```plaintext
["http://example.com"]
```

Für komplexere HTML-Parsing-Anforderungen sollten Sie die `pandoc` Bibliothek in Betracht ziehen, insbesondere wenn Sie mit der Konvertierung von Dokumenten arbeiten. Sie ist außerordentlich vielseitig, kommt aber mit mehr Komplexität:

```haskell
import Text.Pandoc

-- Angenommen, Sie haben ein Pandoc-Dokument (doc) geladen, z.B. durch das Lesen einer Datei
let doc = ... -- Ihr Pandoc-Dokument kommt hier hin

-- Das Dokument in einen HTML-String konvertieren
let htmlString = writeHtmlString def doc

-- Nun würden Sie `htmlString` wie oben parsen oder wie für Ihre Anforderungen erforderlich fortfahren.
```
Beachten Sie, dass `pandoc` eine wesentlich größere Bibliothek ist, die sich auf die Konvertierung zwischen zahlreichen Auszeichnungsformaten konzentriert. Verwenden Sie sie also, wenn Sie diese zusätzlichen Fähigkeiten benötigen oder wenn Sie bereits in Ihrer Anwendung mit Dokumentformaten umgehen.
