---
title:                "Envoi d'une requête http"
html_title:           "Haskell: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

En programmation, envoyer une requête HTTP signifie communiquer avec un serveur à l'aide du protocole HTTP. Cela permet aux développeurs de récupérer des données à partir d'un serveur et de les utiliser dans leur application. Les requêtes HTTP sont couramment utilisées pour créer des applications web et mobiles.

# Comment faire:

Voici un exemple de code en Haskell pour envoyer une requête HTTP à l'aide du package `http-client` :

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://www.example.com"
    print $ getResponseStatusCode response
```

Cela va envoyer une requête GET à `www.example.com` et afficher le code de statut de la réponse.

# Plongée en profondeur:

Les requêtes HTTP ont été développées en 1991 pour permettre aux systèmes informatiques de communiquer entre eux sur internet. Il existe plusieurs alternatives à HTTP, telles que HTTPS, FTP et SMTP. 
Les détails d'implémentation de l'envoi d'une requête HTTP peuvent varier selon la bibliothèque utilisée et le langage de programmation. Dans Haskell, le package `http-client` utilise des types de données stricts pour optimiser les performances lors de l'envoi d'une requête.

# Voir aussi:

- Documentation officielle de `http-client` : https://hackage.haskell.org/package/http-client
- Tutoriel sur les requêtes HTTP avec Haskell : https://hackernoon.com/http-requests-and-json-parsing-in-haskell-95898679fab5 
- Comparaison entre HTTP et HTTPS : https://www.cloudflare.com/en-gb/learning/ddos/what-is-http-https/