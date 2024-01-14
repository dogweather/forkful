---
title:                "Haskell: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

La tecnica di parsing HTML è fondamentale per estrarre informazioni strutturate da pagine web. Ciò consente agli sviluppatori di creare bot e applicazioni di scraping, utili per l'analisi di dati e l'automazione di alcune attività.

## Come

Per esempio, possiamo utilizzare la libreria `html-conduit` per effettuare il parsing di un documento HTML in Haskell. Supponiamo di voler estrarre il contenuto presente all'interno di un tag specifico, come ad esempio il titolo di una notizia.

```
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Text.HTML.DOM as DOM
import qualified Network.HTTP.Conduit as HTTP

fetchHTML :: IO T.Text
fetchHTML = do
  request <- HTTP.parseUrlThrow "https://www.example.com/notizia"
  response <- HTTP.withManager $ HTTP.httpLbs request
  return $ T.decodeUtf8 $ HTTP.responseBody response

main :: IO ()
main = do
  html <- fetchHTML
  let doc = DOM.parseLBS html
      title = C.runConduitRes $
        doc C... C.element "h1" C..| C.lmap DOM.content C..| C.sinkList
  putStrLn $ T.unpack $ head title
```

In questo esempio, utilizziamo la libreria `Text.HTML.DOM` per convertire il documento HTML in un tipo di dati `Document`, che rappresenta l'albero di parsing. Successivamente, utilizziamo le funzioni della libreria `Conduit` per navigare all'interno dell'albero e trovare il contenuto del tag `h1`. Infine, stampiamo il valore ottenuto.

Output:

```
"Covid: Italia in zona bianca da lunedì, ecco le nuove regole"
```

## Approfondimento

Questa è solo una semplice dimostrazione di come si possa utilizzare la libreria `html-conduit` per effettuare il parsing di un documento HTML. Tuttavia, ci sono molte altre librerie e approcci che possono essere utilizzati per gestire il parsing di pagine web in Haskell.

Inoltre, è importante tenere presente che il parsing di HTML non è un'operazione banale. Un documento HTML può contenere molte irregolarità e diversi stili di scrittura, il che rende necessario un approccio robusto per gestirle.

## Vedi anche

- [Haskell Weekly: Parsing HTML](https://haskellweekly.news/issues/issue-184.html)
- [html-conduit library documentation](https://hackage.haskell.org/package/html-conduit)
- [Conduit library documentation](https://hackage.haskell.org/package/conduit)