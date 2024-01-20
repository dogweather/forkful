---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 
L’envoi d’une requête HTTP avec une authentification de base est un moyen de se connecter à des sites web protégés. Les programmeurs l’utilisent pour gérer l’accès aux ressources web.

## Comment faire :
Nous allons utiliser le paquet `http-conduit` pour cela. Voici un exemple montrant comment envoyer une requête GET avec l'authentification de base.

```haskell
import Network.HTTP.Conduit
import Network.HTTP.Client (applyBasicAuth)
import Data.ByteString.Char8 (pack)

main :: IO ()
main = do
    initReq <- parseUrlThrow "http://example.com"
    let req = applyBasicAuth (pack "username") (pack "password") initReq
    manager <- newManager tlsManagerSettings
    res <- httpLbs req manager
    print res
```

## Plongée profonde
Auparavant, nous utilisions le paquet `http`, mais il a été rendu obsolète par `http-conduit`, car il gère les connexions TLS et les pools de connexions, entre autres.

Il existe d'autres moyens d'envoyer une requête HTTP avec authentification de base, comme l'utilisation des paquets `http-client` et `http-client-tls`.

Les détails d'implémentation de l'envoi d'une requête HTTP avec authentification de base comprennent la création d'une `Request` initiale, l'application d'une authentification de base à la requête et l'envoi de la requête à l'aide d'un `Manager`.

## Voir aussi
Pour plus d'informations sur l'envoi de demande HTTP avec authentification de base en Haskell, jetez un coup d'œil à ces liens :
- Documentation http-conduit : https://www.stackage.org/haddock/lts-8.24/http-conduit-2.2.4/Network-HTTP-Conduit.html
- Guide pratique sur les demandes HTTP en Haskell : https://seanhess.github.io/2015/08/04/practical-haskell-http.html
- GitHub http-conduit : https://github.com/snoyberg/http-client