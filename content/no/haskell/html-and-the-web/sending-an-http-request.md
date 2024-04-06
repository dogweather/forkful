---
date: 2024-01-20 17:59:58.335257-07:00
description: 'Hvordan: Eksempelutdata.'
lastmod: '2024-04-05T21:53:41.810014-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Hvordan:
```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://httpbin.org/get"
    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    putStrLn $ "Response body: " ++ show (getResponseBody response)
```

Eksempelutdata:

```
Status code: 200
Response body: "{\n  \"args\": {}, \n  \"headers\": {...}, \n  \"origin\": \"x.x.x.x\", \n  \"url\": \"https://httpbin.org/get\"\n}\n"
```

## Dypdykk:
I de gode gamle dager, kommuniserte programmer oftest lokalt. Internettets vekst har endret dette. Å sende en HTTP-forespørsel har blitt standarden for internettbasert kommunikasjon. Det er flere HTTP-biblioteker i Haskell, som `http-simple`, `wreq`, og `http-conduit`. Disse abstraherer bort de lavnivå detaljene i å lage HTTP-forespørsler.

`http-simple` gir en enkel API for å sende HTTP-forespørsler og håndtere respons. Det bruker `http-client` under panseret, og håndterer mye av komplikasjonene for deg, som å arbeide med nettverksforbindelser og parsing av responsdata. Du kan også bruke `http-conduit` for en mer strømmet, minneeffektiv behandling av store responser.

## Se Også:
- [http-simple documentation](https://hackage.haskell.org/package/http-conduit)
- [http-conduit package](https://hackage.haskell.org/package/http-conduit)
- [wreq package](https://hackage.haskell.org/package/wreq)
- [Learn You a Haskell for Great Good! - an accessible introduction to Haskell](http://learnyouahaskell.com/)
- [Real World Haskell - a book geared towards practical Haskell programming](http://book.realworldhaskell.org/)
