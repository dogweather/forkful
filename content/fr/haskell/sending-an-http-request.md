---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Dans le développement web, l'envoi d'une requête HTTP est une étape commune. C'est une demande d'accès à des informations hébergées sur un serveur. Les programmeurs envoient ces requêtes pour récupérer, poster, mettre à jour ou supprimer des données.

## Comment faire:

Haskell, grâce à `http-client` et `http-conduit`, vous permet d'envoyer des requêtes HTTP simplement. Voyons comment:
 
```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "Le statut de la réponse HTTP est " ++ show (getResponseStatusCode response)
    putStrLn $ "Les en-têtes de la réponse HTTP sont " ++ show (getResponseHeaders response)
    putStrLn $ "Le corps de la réponse HTTP est " ++ show (getResponseBody response)
```

Ce code envoie une requête HTTP GET à `http://httpbin.org/get` et imprime le statut, les en-têtes, et le corps de la réponse.

## Plongeon en profondeur

Créé dans les années 1990, le protocole HTTP est devenu la fondation de toute communication de données sur le web. L'envoi de requêtes HTTP est une partie fondamentale de ce protocole.

Haskell propose d'autres bibliothèques pour gérer les requêtes HTTP comme `http-streams` et `wreq`. L'utilisation dépend des besoins et préférences du projet.

"Http-client" est un paquet Haskell qui recherche la simplicité et la flexibilité. Il peut fonctionner avec ou sans conduit. 

## Voir Aussi

Si vous souhaitez approfondir le sujet, consultez les ressources suivantes :

1. Le dépôt GitHub pour `http-client` : [http-client on GitHub](https://github.com/snoyberg/http-client)
2. Un guide approfondi pour `http-conduit` : [http-conduit Guide](https://www.stackage.org/haddock/lts-9.1/http-conduit-2.2.3.2/Network-HTTP-Conduit.html)
3. Le site officiel du protocole HTTP: [HTTP | MDN](https://developer.mozilla.org/fr/docs/Web/HTTP)