---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:12.619816-07:00
description: "Come fare: Per parsare HTML in Haskell, useremo la libreria `tagsoup`\
  \ per la sua semplicit\xE0 e flessibilit\xE0. Prima di tutto, assicurati di installare\
  \ la\u2026"
lastmod: '2024-03-13T22:44:43.472914-06:00'
model: gpt-4-0125-preview
summary: "Per parsare HTML in Haskell, useremo la libreria `tagsoup` per la sua semplicit\xE0\
  \ e flessibilit\xE0."
title: Analisi del HTML
weight: 43
---

## Come fare:
Per parsare HTML in Haskell, useremo la libreria `tagsoup` per la sua semplicità e flessibilità. Prima di tutto, assicurati di installare la libreria aggiungendo `tagsoup` al file cabal del tuo progetto o eseguendo `cabal install tagsoup`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- HTML di esempio per dimostrazione
let sampleHtml = "<html><body><p>Impara Haskell!</p><a href='http://example.com'>Clicca Qui</a></body></html>"

-- Parsare HTML e filtrare per i link (tag a)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- Stampare i link estratti
print links
```

Output di esempio:
```plaintext
["http://example.com"]
```

Per esigenze di parsing HTML più sofisticate, considera l'uso della libreria `pandoc`, specialmente se stai lavorando con la conversione di documenti. È eccezionalmente versatile ma comporta maggiore complessità:

```haskell
import Text.Pandoc

-- Assumendo di avere un documento Pandoc (doc) caricato, ad esempio, dalla lettura di un file
let doc = ... -- Il tuo documento Pandoc va qui

-- Convertire il documento in una stringa HTML
let htmlString = writeHtmlString def doc

-- Ora, dovresti parsare `htmlString` come sopra o procedere secondo le tue necessità.
```
Tieni presente che `pandoc` è una libreria molto più ampia che si concentra sulla conversione tra numerosi formati di markup, quindi usala se hai bisogno di quelle capacità extra o se stai già trattando formati di documenti nella tua applicazione.
