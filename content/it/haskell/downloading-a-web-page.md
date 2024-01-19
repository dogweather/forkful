---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# L'arte del Download di una Pagina Web in Haskell

## Che Cosa & Perché?
Il download di una pagina web è il processo di copia dei dati di una pagina web sul tuo computer locale. I programmatori lo fanno spesso per l'elaborazione offline dei dati o l'analisi web.

## Come Fare:
Per scaricare una pagina web in Haskell, puoi usare la libreria http-conduit. Ecco un esempio di codice.

```Haskell
import Network.HTTP.Conduit
import Data.ByteString as B

main :: IO ()
main = do
  putStrLn "Scarica una pagina web in Haskell"
  siteData <- simpleHttp "http://www.example.com"
  B.writeFile "example.html" siteData
  putStrLn "Fatto, controlla il tuo file example.html."
```

Quando esegui il codice sopra, scaricherà il HTML dalla pagina www.example.com e lo salverà nel file 'example.html'.


## Approfondimento:
La libreria http-conduit è recente rispetto alla maggior parte delle altre librerie HTTP di Haskell. È nata dall'evoluzione delle pratiche di programmazione di rete con Haskell ed è attualmente una delle scelte più popolari per tale scopo. Altre opzioni sono la libreria http ed http-streams, ma http-conduit è più versatile.

In termini di dettagli di implementazione, `simpleHttp` esegue una richiesta GET al sito web specificato e restituisce i dati come `ByteString`. ByteString è più efficiente in termini di memoria rispetto a String, motivo per cui viene utilizzata in situazioni come questa dove i dati possono essere grandi.


## Altre Risorse:
1. Documentazione della libreria http-conduit: [link](http://hackage.haskell.org/package/http-conduit)
2. Un tutorial su come effettuare richieste HTTP in Haskell: [link](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/tagsoup).
3. Il pacchetto http di Haskell: [link](http://hackage.haskell.org/package/HTTP)
4. Il pacchetto http-streams di Haskell: [link](http://hackage.haskell.org/package/http-streams)