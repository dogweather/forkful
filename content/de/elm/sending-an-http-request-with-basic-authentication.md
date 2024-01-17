---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "Elm: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Was & Warum:
Das Senden einer HTTP-Anfrage mit Basisauthentifizierung bedeutet, dass die Anfrage mit grundlegenden Benutzerdaten übermittelt wird, um die Identität des Clients zu überprüfen. Programmierer tun dies, um sicherzustellen, dass nur autorisierte Benutzer auf bestimmte Ressourcen zugreifen können.

Wie geht's:
Elm bietet eine einfache und elegante Möglichkeit, HTTP-Anfragen mit Basisauthentifizierung durchzuführen. Hier ist ein Beispiel, wie man eine Anfrage an https://example.com mit einem Benutzernamen und Passwort senden kann:

'''Elm
import Http
import Json.Decode as Json


type alias Response =
  { status : Int
  , body : Json.Value
  }

type Msg
  = RequestResult (Result Http.Error Response)


authenticate : String -> String -> Cmd Msg
authenticate username password =
  Http.send RequestResult <| Http.post "https://example.com" noBody
    [ Http.basicAuth username password
    ]

-- Aufruf: authenticate "Benutzername" "Passwort"
'''

Durch das Senden einer POST-Anfrage mit der ```Http.basicAuth``` Funktion wird der Benutzername und das Passwort in der Kopfzeile der Anfrage eingefügt, um die Authentifizierung zu ermöglichen.

Eine beispielhafte Antwort könnte so aussehen:

'''Elm
Ok
  { status = 200
  , body = Json.succeed { message = "Erfolgreich authentifiziert." }
  }
'''

Wir erhalten also den Statuscode 200 (OK) und die Anfrage war erfolgreich.

Tieferer Einblick:
Die Basisauthentifizierung wurde in den frühen Tagen des HTTP-Protokolls entwickelt und ist ein einfaches und weit verbreitetes Verfahren zur Identitätsüberprüfung. Alternativen dazu sind beispielsweise OAuth oder OpenID.

Die Implementierung von HTTP-Anfragen mit Basisauthentifizierung in Elm basiert auf der Verwendung von einem HTTP-Header namens "Authorization", der den Benutzernamen und das Passwort in einem Base64-kodierten Format enthält.

Schau auch hier:
- Offizielle Elm Dokumentation zu HTTP-Anfragen: http://package.elm-lang.org/packages/elm-lang/http/latest/Http
- Mehr Informationen zu Basic-Authentifizierung: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication