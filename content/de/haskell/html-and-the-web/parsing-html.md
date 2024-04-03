---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:38.570923-07:00
description: "Wie geht das: Um HTML in Haskell zu parsen, verwenden wir die `tagsoup`\
  \ Bibliothek aufgrund ihrer Einfachheit und Flexibilit\xE4t. Stellen Sie zun\xE4\
  chst\u2026"
lastmod: '2024-03-13T22:44:53.929360-06:00'
model: gpt-4-0125-preview
summary: "Um HTML in Haskell zu parsen, verwenden wir die `tagsoup` Bibliothek aufgrund\
  \ ihrer Einfachheit und Flexibilit\xE4t."
title: HTML parsen
weight: 43
---

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
