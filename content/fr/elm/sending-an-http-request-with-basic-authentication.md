---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?

L'envoi d'une requête HTTP avec authentification de base implique d'envoyer des informations d'identification dans le corps de la requete. Les programmeurs font cela pour sécuriser certaines parties de l'application web en ne permettant l'accès qu'aux utilisateurs autorisés.

## Comment faire:

Elm donne une manière propre et structurée pour gérer les requêtes HTTP. Regardons un exemple de base.

```Elm
import Http
import Json.Decode as Decode

type Msg = GotUser (Result Http.Error String)

userDecoder : Decode.Decoder String
userDecoder = Decode.field "username" Decode.string

getUser : Cmd Msg
getUser =
    Http.get
        { url = "http://my-api/users/1"
        , expect = Http.expectJson GotUser userDecoder
        }
```
Avez-vous remarqué `Http.expectJson GotUser userDecoder`? C'est là que nous définissons comment la réponse devrait être décodée. Si la requête est réussie, elle renvoie un `Result` contenant soit une `String` (si tout s'est bien passé), soit une `Http.Error`.

Maintenant, pour ajouter une authentification de base à cette requête, nous devons ajouter un en-tête d'autorisation. Voilà comment on fait :

```Elm
import Http
import Http.Header as Header
import Json.Decode as Decode

type Msg = GotUser (Result Http.Error String)

userDecoder : Decode.Decoder String
userDecoder = Decode.field "username" Decode.string

getUser : Cmd Msg
getUser =
    Http.request
        { method = "GET"
        , headers = [ Header.authorization "Basic QWxhZGRpbjpPcGVuU2VzYW1l" ]
        , url = "http://my-api/users/1"
        , body = Http.emptyBody
        , expect = Http.expectJson GotUser userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
```
Notez le `"Basic QWxhZGRpbjpPcGVuU2VzYW1l"` - c'est une chaîne codée en base64 qui représente les informations d'identification.

## Prolongement

Historiquement, l'authentification de base a été une de premières méthodes implémentées pour sécuriser les transferts HTTP. Aujourd'hui, elle reste une méthode populaire grâce à sa simplicité malgré l'existence d'autres alternatives plus sécurisées comme l'authentification par jeton ou OAuth.

En Elm, le standard pour envoyer des requêtes HTTP est le module `Http`, mais plusieurs libraries comme `elm-http-builder` sont également disponibles pour simplifier ces processus.

## Voir aussi

Documentation de Elm HTTP : [lien](https://package.elm-lang.org/packages/elm/http/latest/)

Code HTTP de base dans Elm: [lien](https://elmprogramming.com/elm-http.html)

L'authentification de base HTTP: [lien](https://www.ietf.org/rfc/rfc2617.txt)

Elm-http-builder : [lien](https://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest/)