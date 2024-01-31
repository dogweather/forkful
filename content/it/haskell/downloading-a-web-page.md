---
title:                "Scaricare una pagina web"
date:                  2024-01-20T17:44:10.661467-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Scaricare una pagina web significa ottenere il contenuto HTML di quella pagina via internet. I programmatori lo fanno per analizzare dati, testare siti o automatizzare attività online.

## How to:
In Haskell, usiamo librerie come `http-conduit` per scaricare pagine web. Ecco un semplice esempio:

```Haskell
import Network.HTTP.Simple (httpBS, getResponseBody)

main :: IO ()
main = do
  response <- httpBS "http://example.com"
  let body = getResponseBody response
  putStrLn $ "Il contenuto della pagina è: " ++ show body
```

Output di esempio:
```
Il contenuto della pagina è: "<html>...</html>"
```

Prima di eseguire questo codice, installa il pacchetto `http-conduit` con `cabal install http-conduit`.

## Deep Dive:
Scaricare pagine web in Haskell non è difficile, ma richiede di comprendere alcune basi. `http-conduit` è una scelta popolare dato che gestisce automatizzazioni come la gestione dei cookies e reindirizzamenti. In passato, si usavano librerie come `HTTP` o `curl`. Oltre a scaricare semplici pagine HTML, potresti voler gestire anche JSON o XML, usando librerie come `aeson` o `xml-conduit`. Quando implementi il download, considera gli header HTTP, le richieste POST, e la gestione degli errori di rete.

## See Also:
- Documentazione di `http-conduit`: https://hackage.haskell.org/package/http-conduit
- Libreria `aeson` per JSON: https://hackage.haskell.org/package/aeson
- Libreria `xml-conduit` per XML: https://hackage.haskell.org/package/xml-conduit
- Tutorial su HTTP in Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/http-client
