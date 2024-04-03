---
date: 2024-01-20 17:59:40.763419-07:00
description: "How to: Haskell est \xE9l\xE9gant. Utilisons `http-client` pour envoyer\
  \ une requ\xEAte GET simple."
lastmod: '2024-03-13T22:44:57.830477-06:00'
model: gpt-4-1106-preview
summary: "Haskell est \xE9l\xE9gant."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## How to:
Haskell est élégant. Utilisons `http-client` pour envoyer une requête GET simple.

```Haskell
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    putStrLn $ "Le statut: " ++ show (statusCode $ responseStatus response)
    print $ responseBody response
```

Output:
```
Le statut: 200
"{\"args\":{},\"headers\":{...},\"origin\":\"...\",\"url\":\"http://httpbin.org/get\"}"
```
Simple, non?

## Deep Dive:
Historiquement, `http-client` est la bibliothèque go-to pour HTTP avec Haskell, manifestant l'approche minimaliste de Haskell. Pour HTTP/2 ou des fonctions multithreading, regardez `http2-client`. Les détails comme la gestion des headers ou des cookies sont fins, mais la doc couvre ça en profondeur.

## See Also:
- La documentation de `http-client`: http://hackage.haskell.org/package/http-client
- Pour des requêtes plus complexes, `servant-client`: http://hackage.haskell.org/package/servant-client
- Les RFCs HTTP, pour comprendre ce qui se passe sous le capot: https://tools.ietf.org/html/rfc7231
