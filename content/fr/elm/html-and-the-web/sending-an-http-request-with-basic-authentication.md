---
title:                "Envoi d'une requête HTTP avec authentification de base"
aliases: - /fr/elm/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:32.928982-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
En Elm, envoyer une requête HTTP avec une authentification de base consiste à transmettre des identifiants (nom d'utilisateur et mot de passe) pour accéder à des ressources protégées. On le fait souvent pour interagir avec des API sécurisées qui nécessitent une identification.

## Comment faire :
```Elm
import Http
import Base64

type Msg
    = GotData (Result Http.Error String)

basicAuth : String -> String -> List Http.Header
basicAuth username password =
    let
        credentials = username ++ ":" ++ password
        encodedCredentials = Base64.encode credentials
    in
    [ Http.header "Authorization" ("Basic " ++ encodedCredentials) ]

getProtectedResource : Cmd Msg
getProtectedResource =
    Http.get
        { url = "https://your-protected-resource.com"
        , headers = basicAuth "yourUsername" "yourPassword"
        , expect = Http.expectString GotData
        }

-- Insérer getProtectedResource là où il est pertinent, comme dans un effet d'initialisation
```

## Exploration approfondie
Historiquement, l'authentification de base HTTP était une des méthodes les plus simples pour sécuriser l'accès aux web services. Aujourd'hui, elle est moins recommandée en raison de sa simplicité et parce qu'elle est moins sécuritaire que des alternatives comme OAuth. Dans Elm, l’implémentation requiert qu’on encode les identifiants en Base64 et ajoute le header d'autorisation de façon manuelle. Elm 0.19 a poussé à un meilleur typage et à la sécurité des interactions HTTP, sans pour autant avoir des spécificités dans la bibliothèque standard pour l'authentification de base, d'où la nécessité de traiter les headers directement.

## Voir également
- Documentation sur `Http` dans Elm : https://package.elm-lang.org/packages/elm/http/latest/
- Encodage Base64 en Elm : https://package.elm-lang.org/packages/truqu/elm-base64/latest/
- Authentification de base HTTP (en anglais) : https://en.wikipedia.org/wiki/Basic_access_authentication
