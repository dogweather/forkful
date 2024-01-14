---
title:                "Elm: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Pourquoi utiliser des requêtes HTTP avec une authentication basique en Elm?

Si vous travaillez sur une application web avec Elm, il est possible que vous deviez communiquer avec une API externe. Dans certains cas, cette API pourrait nécessiter une authentication pour vous donner accès aux données. C'est là que les requêtes HTTP avec une authentication basique deviennent essentielles.

## Comment faire

Pour envoyer une requête HTTP avec une authentication basique en Elm, il existe quelques étapes à suivre. Tout d'abord, vous devez définir les paramètres de votre requête, tels que l'URL et la méthode (GET, POST, etc.). Ensuite, vous devez ajouter vos informations d'authentication dans l'header de la requête. Enfin, vous pouvez envoyer la requête et traiter la réponse.

```Elm
import Http
import Json.Decode exposing (..)

type alias Auth = 
  { username : String
  , password : String
  }

authenticate : Auth -> Http.Request a -> Http.Request a
authenticate auth request =
  Http.header "Authorization" (basicAuth auth.username auth.password) request

basicAuth : String -> String -> String
basicAuth username password =
  "Basic " ++ Base64.encode (username ++ ":" ++ password)
  
requestUrl : String
requestUrl = "https://api.example.com/users"

request : Http.Request (List User)
request =
  Http.get requestUrl userDecoder

userDecoder : Decoder (List User)
userDecoder =
  list (field "name" string
    

authenticatedRequest : Http.Request (List User)
authenticatedRequest = 
  Http.send UserDecoder (authenticate { username = "username", password = "password"})
```

Lorsqu'on envoie la requête `authenticatedRequest`, on obtiendra une réponse contenant une liste d'utilisateurs, décodée grâce à la fonction `userDecoder`.

## Plongée en profondeur

Maintenant que nous avons vu un exemple concret de l'utilisation de requêtes HTTP avec une authentication basique en Elm, il est important de comprendre en profondeur comment cela fonctionne.

Le processus d'authentication basique implique d'ajouter une en-tête `Authorization` dans la requête HTTP. Cette en-tête contient une chaine encodée en base64 qui contient le nom d'utilisateur et le mot de passe, séparés par un `:`. Ainsi, l'API externe pourra vérifier ces informations pour autoriser ou non l'accès.

De plus, il est important de noter que l'utilisation de Base64 pour encoder les informations d'authentication n'est pas considérée comme totalement sécurisée, car il est possible de convertir facilement cette chaine en clair. Il est donc essentiel de sécuriser l'application web et de ne pas stocker les informations d'authentication dans le code.

# Voir aussi

- [Documentation officielle sur les requêtes HTTP en Elm](https://guide.elm-lang.org/effects/http.html)
- [Comprendre l'authentication basique en HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)