---
date: 2024-01-20 17:59:49.199568-07:00
description: "So geht's: In Haskell benutzt man oft die Bibliothek `http-client` f\xFC\
  r HTTP-Anfragen. Hier ein einfaches Beispiel."
lastmod: '2024-03-13T22:44:53.928181-06:00'
model: gpt-4-1106-preview
summary: "In Haskell benutzt man oft die Bibliothek `http-client` f\xFCr HTTP-Anfragen."
title: Einen HTTP-Request senden
weight: 44
---

## So geht's:
In Haskell benutzt man oft die Bibliothek `http-client` für HTTP-Anfragen. Hier ein einfaches Beispiel:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    putStrLn $ "Statuscode: " ++ (show . statusCode . responseStatus $ response)
    putStrLn $ "Antwortkopf: " ++ (show . responseHeaders $ response)
    print $ responseBody response
```

Ausgabe könnte so aussehen:

```
Statuscode: 200
Antwortkopf: [("Content-Type", "application/json"), ...]
{ "args": {}, ... }
```

## Deep Dive:
`http-client` ist praktisch der Standard in Haskell für HTTP. Früher gab es `HTTP`. `http-client` ist moderner, flexibler. Es unterstützt auch HTTPS, automatisches Handling von Cookies und umfassende Konfigurationsmöglichkeiten.

Funktionell gesehen, sendet `httpLbs` eine "lazy" ByteString Antwort zurück – praktisch für große Daten. Alternative Libraries wie `Wreq` oder `Req` bieten ähnliche Funktionalitäten mit etwas anderen API-Designs.

Um nur zu schießen und zu vergessen, gibt es `httpNoBody` – keine Antwortdaten, nur der Status-Code. Für umfangreichere Interaktionen kann man `Request`-Objekte anpassen: Headers setzen, Request-Methode ändern usw.

## Siehe Auch:
- `http-client` Dokumentation: https://www.stackage.org/package/http-client
- `Wreq`: http://www.serpentine.com/wreq/
- `Req`: https://hackage.haskell.org/package/req
- Real World Haskell Buch, HTTP Kapitel: http://book.realworldhaskell.org/read/programming-with-monads.html#id664176
