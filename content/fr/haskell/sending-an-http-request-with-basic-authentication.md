---
title:                "Haskell: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
 
L'envoi de requêtes HTTP avec une authentification de base est une manière sûre et efficace de sécuriser les échanges de données entre un client et un serveur. Cela est particulièrement utile lorsqu'une application nécessite un accès restreint à certaines ressources.

## Comment faire

Pour envoyer une requête HTTP avec une authentification basique en Haskell, vous pouvez utiliser la bibliothèque HTTP-client. Voici un exemple de code pour envoyer une requête GET avec un nom d'utilisateur et un mot de passe :

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
  request <- parseRequest "https://www.example.com"
  manager <- newManager tlsManagerSettings
  let username = "user"
  let password = "pass"
  let auth = basicAuthHeader username password
  response <- httpLbs request { requestHeaders = [auth] } manager
  putStrLn $ "Status code: " ++ show (statusCode $ responseStatus response)
  putStrLn $ "Response body: " ++ (unpack $ responseBody response)
```

L'exemple ci-dessus utilise la fonction `basicAuthHeader` pour générer l'en-tête d'authentification basique en utilisant le nom d'utilisateur et le mot de passe fournis. Vous pouvez également utiliser cette fonction pour envoyer des requêtes POST et spécifier des paramètres d'authentification supplémentaires, tels que la méthode de codage (encode) et la méthode de hachage (algorithme).

## Plongée en profondeur

Les demandes HTTP avec une authentification basique sont sécurisées car l'en-tête d'authentification est encodée en utilisant la méthode de codage de base64. Néanmoins, il est important de noter que cette méthode n'offre pas un niveau de sécurité élevé car les informations d'identification sont envoyées en clair et peuvent être facilement décodées si elles sont interceptées.

Pour une sécurité renforcée, il est recommandé d'utiliser l'authentification basique combinée avec SSL (Secure Sockets Layer) ou TLS (Transport Layer Security) pour chiffrer les données lors de leur transmission.

## Voir aussi

- [Documentation de la bibliothèque HTTP-client](https://hackage.haskell.org/package/http-client)
- [Tutorial sur les requêtes HTTP en Haskell](https://haskell-lang.org/tutorial)
- [Introduction à l'authentification basique](https://developer.mozilla.org/fr/docs/Web/HTTP/Basic_access_authentication)