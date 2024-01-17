---
title:                "Envoyer une demande http avec une authentification basique."
html_title:           "Haskell: Envoyer une demande http avec une authentification basique."
simple_title:         "Envoyer une demande http avec une authentification basique."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
Lorsque vous interagissez avec un serveur en ligne, il peut être nécessaire d'envoyer une demande HTTP avec une authentification de base. Cela signifie que vous envoyez une demande avec un nom d'utilisateur et un mot de passe pour accéder à un service spécifique. Les programmeurs le font pour s'assurer que seules les personnes autorisées peuvent accéder à certaines fonctionnalités ou données.

## Comment faire:
Pour envoyer une demande HTTP avec une authentification de base en Haskell, vous pouvez utiliser la bibliothèque Network.HTTP.Simple. Voici un exemple de code montrant comment envoyer une demande GET avec une authentification de base :

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS

request :: Request
request = setRequestBasicAuth "username" "password" "http://example.com"

main :: IO ()
main = do
  response <- httpLBS request
  BS.putStrLn $ getResponseBody response
```

Lorsque vous exécutez ce code, vous devriez voir la réponse de votre demande imprimée dans la console. Assurez-vous de remplacer "username" et "password" par vos propres informations d'identification.

## Plongée Profonde:
Cette méthode d'authentification est appelée "authentification de base" car elle était l'une des premières méthodes d'authentification utilisées pour les applications en ligne. Elle n'est pas considérée comme très sécurisée car elle envoie les informations d'identification en texte clair, sans aucun chiffrement. Les alternatives modernes incluent des méthodes telles que OAuth et OpenID Connect.

## Voir Aussi:
Si vous souhaitez en apprendre davantage sur l'envoi de demandes HTTP avec une authentification de base en Haskell, voici quelques ressources utiles:

- [Documentation de la bibliothèque Network.HTTP.Simple](https://hackage.haskell.org/package/http-client-0.7.1/docs/Network-HTTP-Simple.html)
- [Un tutoriel détaillé sur l'utilisation de Network.HTTP.Simple](https://www.snoyman.com/blog/2017/04/introducing-http-client)
- [Un exemple de mise en œuvre de l'authentification de base avec la bibliothèque Network.HTTP.Conduit](https://www.fpcomplete.com/blog/2019/12/iosched-haskell-tutorial)