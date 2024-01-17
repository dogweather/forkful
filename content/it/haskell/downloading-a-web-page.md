---
title:                "Scaricare una pagina web"
html_title:           "Haskell: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che cos'è e perché scaricare una pagina web?

Scaricare una pagina web significa ottenere il codice sorgente di una pagina web da un server tramite una connessione Internet. I programmatori spesso scaricano pagine web per analizzare il codice o estrarre informazioni utili.

## Come fare:

```Haskell 
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client

main :: IO ()
main = do
  request <- parseRequest "https://www.example.com"
  response <- httpLbs request
  print $ getResponseStatusCode response
  print $ getResponseHeader "Content-Type" response
  L.putStr $ getResponseBody response
```

Esempio di output:

```Haskell 
200
Just "text/html; charset=utf-8"
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
...
```

## Approfondiamo:

Scaricare pagine web è un'operazione comune nella programmazione moderna. Ci sono diverse librerie disponibili in Haskell per farlo, come ad esempio `http-client` e `http-conduit`. Inoltre, è possibile utilizzare anche strumenti come `curl` e `wget` da riga di comando.

Un aspetto importante da tenere in considerazione quando si scaricano pagine web è il trattamento dei dati sensibili, come ad esempio password e token di autenticazione. E' importante utilizzare protocolli sicuri come HTTPS e gestire correttamente la sicurezza dei dati.

## Vedi anche:

- [Hackage - HTTP Client](https://hackage.haskell.org/package/http-client)
- [Hackage - HTTP Conduit](https://hackage.haskell.org/package/http-conduit)
- [CURL](https://curl.haxx.se/)
- [Wget](https://www.gnu.org/software/wget/)