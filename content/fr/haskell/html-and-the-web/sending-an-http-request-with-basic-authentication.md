---
date: 2024-01-20 18:01:40.513552-07:00
description: "L\u2019envoi d\u2019une requ\xEAte HTTP avec authentification basique\
  \ consiste \xE0 transmettre \xE0 un serveur des identifiants (nom d\u2019utilisateur\
  \ et mot de passe) encod\xE9s\u2026"
lastmod: 2024-02-19 22:05:16.571093
model: gpt-4-1106-preview
summary: "L\u2019envoi d\u2019une requ\xEAte HTTP avec authentification basique consiste\
  \ \xE0 transmettre \xE0 un serveur des identifiants (nom d\u2019utilisateur et mot\
  \ de passe) encod\xE9s\u2026"
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L’envoi d’une requête HTTP avec authentification basique consiste à transmettre à un serveur des identifiants (nom d’utilisateur et mot de passe) encodés en base64 dans les en-têtes HTTP afin de s'identifier. Les programmeurs utilisent cela pour accéder à des ressources protégées sur un serveur qui nécessite une vérification de l'identité.

## Comment faire :

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as Char8

-- Encoder les identifiants
let username = "user"
let password = "pass"
let encodedCredentials = encode . Char8.pack $ username ++ ":" ++ password

-- Création de l'en-tête d'autorisation
let authHeader = (hAuthorization, "Basic " <> encodedCredentials)

-- Construction de la requête
let request = setRequestHeader authHeader $ defaultRequest
                   { host = "example.com"
                   , path = "/protected/resource"
                   }

-- Envoi de la requête
response <- httpLBS request

-- Affichage du code et du corps de la réponse
putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
putStrLn $ "Body: " ++ Char8.unpack (getResponseBody response)
```

## Exploration approfondie

Le concept d’authentification basique pour les requêtes HTTP n’est pas récent. Il est défini dans le RFC 7617, et bien qu'il soit simple à implémenter, il n'est pas le plus sécurisé car les identifiants encodés en base64 peuvent être facilement décodés. Citons comme alternatives plus sécurisées l'authentification Digest ou les tokens d’authentification comme OAuth.

En Haskell, utiliser `Network.HTTP.Simple` pour les requêtes HTTP est un choix populaire car il offre une API simple. L'ajout de l'en-tête d'autorisation est direct avec le package `http-types`, et l'encodage des identifiants utilise `Data.ByteString.Base64`, ce qui garantit que les données sont correctement formées pour l'envoi.

## Voir aussi

- Documentation de `Network.HTTP.Simple` : https://hackage.haskell.org/package/http-conduit/docs/Network-HTTP-Simple.html
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- Informations sur OAuth : https://oauth.net/
