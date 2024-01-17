---
title:                "Analizzare l'html."
html_title:           "Haskell: Analizzare l'html."
simple_title:         "Analizzare l'html."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il parsing HTML è il processo di analisi e interpretazione del codice HTML di una pagina web. I programmatori lo fanno per estrarre informazioni specifiche dalla pagina, come testo, link, immagini e altro ancora.

## Come fare:
```Haskell
import Text.HTML.TagSoup

-- Esempio di code per la pagina di Google
tags = parseTags "<html><body><h1>Hello, World!</h1></body></html>"

-- Estrae il testo all'interno del tag <h1>
headings = filter isTagText tags
output = unwords $ map fromTagText headings
-- Output: "Hello, World!"
```

## Approfondimento:
Il parsing HTML è stato introdotto per la prima volta nei primi anni '90 come uno dei principali linguaggi di marcatura utilizzati per la creazione delle prime pagine web. Oggi ci sono molti strumenti diversi disponibili per il parsing HTML, come Text.HTML.TagSoup e HaXml.

## Vedi anche:
- [Haskell Wiki: Parsing HTML](https://wiki.haskell.org/Parsing_HTML)
- [Haskell TagSoup Library](https://hackage.haskell.org/package/tagsoup)
- [HaXml Library for XML & HTML documents](https://hackage.haskell.org/package/HaXml)