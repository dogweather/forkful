---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/parsing-html.md"
---

{{< edit_this_page >}}

# Analisi HTML (Parsing) con Haskell

Ciao a tutti! Oggi esploreremo insieme come analizzare l'HTML usando Haskell, il paradiso dei linguaggi di programmazione funzionali. 

## Cos'è e perché?

L'analisi HTML consiste nel tradurre il codice HTML, un linguaggio di marcatura, in una struttura dati che il nostro programma può manipolare. Lo facciamo per ottenere o manipolare dati da pagine web.

## Come fare:

Iniziamo con l'installare la libreria `tagsoup` su Haskell:

```Haskell
cabal install tagsoup
```

Ora possiamo scrivere un semplice programma di analisi. Ecco un esempio:

```Haskell
import Text.HTML.TagSoup

htmlParser :: String -> [Tag String]
htmlParser = parseTags

main = do
  let tags = htmlParser "<title>Benvenuto alla programmazione Haskell!</title>"
  print tags
```

Questo parser stampa una lista di tag HTML. Ecco l'output:
```Haskell
[TagOpen "title" [],TagText "Benvenuto alla programmazione Haskell!",TagClose "title"]
```

## Approfondimento

Una volta, l'HTML era analizzato principalmente con espressioni regolari, ma questo metodo ha i suoi limiti. Haskell, nato nel lontano 1990, ha reso l'analisi molto più semplice e sicura con l'uso di parser componibili.

Un'alternativa a `tagsoup` è `html-conduit`, che funziona bene con lo streaming di dati.

Dettagli di implementazione: `tagsoup` non fa un'analisi rigorosa. Ignora gli errori di sintassi HTML e cerca di produrre un output utile nonostante il markup malformato. Questo è utile per lavorare con l'HTML del mondo reale.

## Per saperne di più

- Documentation: [TagSoup](https://hackage.haskell.org/package/tagsoup)
- Alternative: [html-conduit](https://hackage.haskell.org/package/html-conduit)