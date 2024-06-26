---
date: 2024-01-20 18:00:06.572478-07:00
description: "How to: I Haskell anv\xE4nds ofta biblioteket `http-conduit` f\xF6r\
  \ att skicka HTTP-beg\xE4ran. H\xE4r \xE4r hur du g\xF6r en enkel GET-beg\xE4ran."
lastmod: '2024-03-13T22:44:37.952016-06:00'
model: gpt-4-1106-preview
summary: "I Haskell anv\xE4nds ofta biblioteket `http-conduit` f\xF6r att skicka HTTP-beg\xE4\
  ran."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

## How to:
I Haskell används ofta biblioteket `http-conduit` för att skicka HTTP-begäran. Här är hur du gör en enkel GET-begäran:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"
    putStrLn $ "Statuskod: " ++ show (getResponseStatusCode response)
    putStrLn $ "Svarskropp: " ++ show (getResponseResponseBody response)
```

Kör programmet och förvänta dig något liknande:

```
Statuskod: 200
Svarskropp: "{\"args\":{},\"headers\":{...},\"origin\":\"...\",\"url\":\"http://httpbin.org/get\"}"
```

## Deep Dive
HTTP-begäran har varit grundläggande för webbprogrammering sedan tidiga 90-talet. Alternativ till `http-conduit` inkluderar `http-client` och lågnivåbibliotek som `network`. `http-conduit` använder `http-client` under huven men förenklar hantering av begäran och svar.

För att skicka en POST-begäran och hantera headers använd följande kod:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple

main :: IO ()
main = do
    let request
            = setRequestMethod "POST"
            $ setRequestPath "/post"
            $ setRequestHost "httpbin.org"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestBodyLBS "{\"sample\":\"data\"}"
            $ defaultRequest
    response <- httpLBS request
    putStrLn $ "Statuskod: " ++ show (getResponseStatusCode response)
    putStrLn $ "Svarskropp: " ++ show (getResponseResponseBody response)
```

Detta öppnar för mer kontroll med fler alternativ för konfiguration av begäran.

## See Also
- `http-conduit` dokumentation: https://www.stackage.org/package/http-conduit
- Officiell `http-client` tutorial: https://haskell-lang.org/library/http-client
- Haskell network-programmering: http://book.realworldhaskell.org/read/network-programming.html
