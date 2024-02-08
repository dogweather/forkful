---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
date:                  2024-01-20T18:01:34.905465-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit Basisauthentifizierung heißt, dass du deine Anmeldedaten in einer sicheren, codierten Form mitschickst, um Zugriff auf geschützte Ressourcen zu bekommen. Programmierer nutzen dies, um eine einfache Authentifizierung auf Webservern zu realisieren, wenn größere Sicherheitsmechanismen nicht erforderlich sind.

## So geht's:

Elm macht HTTP-Anfragen mit Basisauthentifizierung simpel. Du musst nur die `Http`-Bibliothek verwenden und die `Headers` entsprechend setzen. Hier ist ein schnelles Beispiel:

```Elm
import Http
import Base64

type alias Credentials =
    { username : String
    , password : String
    }

basicAuthHeader : Credentials -> Http.Header
basicAuthHeader creds =
    let
        encoded =
            Base64.encode (creds.username ++ ":" ++ creds.password)
    in
    Http.header "Authorization" ("Basic " ++ encoded)

getProtectedResource : Credentials -> String -> Cmd msg
getProtectedResource creds url =
    Http.get
        { url = url
        , headers = [ basicAuthHeader creds ]
        }
        -- Geh davon aus, dass die Antwort ein `String` ist.
        |> Http.expectString
```

Stelle sicher, dass du die `elm/http` und `truqu/elm-base64` Pakete in deinem Projekt installiert hast, um die obigen Importe zu nutzen.

## Tiefgang:

Die Verwendung von Basisauthentifizierung geht auf das HTTP/1.0-Protokoll zurück und wird in der RFC 7617 spezifiziert. Während die Methode aufgrund des Fehlens von Verschlüsselung kritisiert wurde, ist es für einfache Authentifizierungsszenarien durchaus üblich. Als Alternative kannst du komplexere Authentifizierungsmechanismen wie OAuth nutzen, welche Tokens statt Benutzerdaten verwenden. Bei der Implementierung in Elm ist zu beachten, dass die Anmeldeinformationen (Benutzername und Passwort) kodiert und im `Authorization` Header gesendet werden müssen. Sicherheitsempfehlungen raten davon ab, Basisauthentifizierung ohne HTTPS zu verwenden, da die Anmeldeinformationen sonst leicht abgefangen werden können.

## Siehe auch:

- Elm HTTP Paket Dokumentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Elm Base64 Paket: [https://package.elm-lang.org/packages/truqu/elm-base64/latest/](https://package.elm-lang.org/packages/truqu/elm-base64/latest/)
- RFC 7617, The 'Basic' HTTP Authentication Scheme: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
