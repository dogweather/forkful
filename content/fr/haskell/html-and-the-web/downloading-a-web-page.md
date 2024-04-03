---
date: 2024-01-20 17:44:17.742533-07:00
description: 'How to: (Comment faire :) .'
lastmod: '2024-03-13T22:44:57.832671-06:00'
model: gpt-4-1106-preview
summary: .
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to: (Comment faire :)
```Haskell
-- On utilise la librairie `http-conduit` pour télécharger une page web.
import Network.HTTP.Simple

-- Fonction simple pour télécharger le contenu d'une URL.
downloadPage :: String -> IO ByteString
downloadPage url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

-- Essayons de télécharger une page.
main :: IO ()
main = do
    content <- downloadPage "http://example.com"
    putStrLn $ "Contenu téléchargé (premiers 100 caractères): " ++ take 100 (show content)
```
Sortie d'échantillon :
```
Contenu téléchargé (premiers 100 caractères): "<!doctype html>\\n<html>\\n<head>\\n    <title>Example Dom
```

## Deep Dive (Plongée en profondeur)
Historiquement, télécharger des pages web en Haskell pouvait impliquer des librairies comme `HTTP` ou `curl` mais `http-conduit` offre une interface plus moderne avec une gestion simplifiée des connexions persistantes et du streaming. Une alternative serait d'utiliser `wget` ou `curl` en ligne de commande, mais travailler en Haskell offre plus de souplesse pour le traitement des données. L'implémentation sous-jacente gère des détails comme le respect du protocole HTTP, la gestion des entêtes et la compression.

## See Also (Voir Aussi)
- Pour en savoir plus sur la librairie `http-conduit`: [http-conduit sur Hackage](https://hackage.haskell.org/package/http-conduit)
- Pour explorer les autres fonctions de téléchargement HTTP en Haskell: [Network.HTTP on Hackage](https://hackage.haskell.org/package/HTTP)
