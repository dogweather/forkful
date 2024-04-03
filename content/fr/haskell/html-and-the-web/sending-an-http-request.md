---
date: 2024-01-20 17:59:40.763419-07:00
description: "En un clin d'oeil: une requ\xEAte HTTP envoie de l'info vers ou en re\xE7\
  oit depuis un serveur web. Les programmeurs l'utilisent pour interagir avec des\
  \ API,\u2026"
lastmod: '2024-03-13T22:44:57.830477-06:00'
model: gpt-4-1106-preview
summary: "En un clin d'oeil: une requ\xEAte HTTP envoie de l'info vers ou en re\xE7\
  oit depuis un serveur web."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## What & Why?
En un clin d'oeil: une requête HTTP envoie de l'info vers ou en reçoit depuis un serveur web. Les programmeurs l'utilisent pour interagir avec des API, collecter des données, automatiser des tâches web. 

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
