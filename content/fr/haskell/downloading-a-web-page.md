---
title:                "Téléchargement d'une page web"
html_title:           "Haskell: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi ?

Télécharger une page web consiste à récupérer le contenu d'une page web et à le stocker localement pour une utilisation ultérieure. Les programmeurs le font souvent pour automatiser des tâches, comme l'extraction de données ou la mise à jour de contenu.

## Comment faire :

Voici un exemple de code en Haskell pour télécharger une page web à l'aide de la bibliothèque "http-conduit". Le code suivant télécharge la page d'accueil de Google et affiche le code HTML correspondant.

```
// Import de la bibliothèque requise
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

// Télécharge la page d'accueil de Google
let url = "http://www.google.com"
request <- parseUrlThrow url
response <- withManager $ httpLbs request
// Afficher le code HTML
L.putStrLn $ responseBody response
```

Output : ```<!doctype html> <html ...```

## Plongée profonde :

Les programmeurs utilisent souvent des bibliothèques telles que "http-conduit" pour télécharger des pages web en raison de leur fiabilité et de leur compatibilité avec les normes web. D'autres options sont également disponibles, telles que "wget" en ligne de commande ou "Selenium" pour les tests automatisés. Les bibliothèques Haskell utilisent généralement des sockets réseau pour établir une connexion avec le serveur web et transférer le code HTML de la page demandée.

## Voir aussi :

- Documentation de la bibliothèque "http-conduit" : https://hackage.haskell.org/package/http-conduit
- Tutoriel pour télécharger une page web en utilisant Haskell : https://www.fpcomplete.com/blog/2017/07/using-http-conduit-downloading-web-pages-haskell
- Comparaison des différentes bibliothèques de téléchargement web en Haskell : https://wiki.haskell.org/Libraries_for_HTTP