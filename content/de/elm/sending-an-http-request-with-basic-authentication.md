---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Senden einer HTTP-Anfrage mit Basisauthentifizierung ist eine gängige Vorgehensweise um zu verifizieren, dass ein Nutzer authorisiert ist, bestimmte Daten oder Dienste zu verwenden. Entwickler verwenden diese Methode, um die Sicherheit von Netzwerkanwendungen zu verbessern und den unberechtigten Zugriff auf sensible Daten zu verhindern.

## Wie werde ich's los?

Jetzt ein einfacher Code, wie man eine HTTP-Anfrage mit Basisauthentifizierung in Elm sendet: 

``` Elm
import Http exposing (..)
import Http.Auth

basicAuth : String -> String -> Http.Header
basicAuth username password =
    Http.header "Authorization" ("Basic " ++ btoa(username ++ ":" ++ password))

request : Http.Request String
request =
    Http.request
        { method = "GET"
        , headers = [ basicAuth "Benutzername" "Passwort" ]
        , url = "https://meine-api.de/daten"
        , body = Http.emptyBody
        , expect = Http.expectString (Ok)
        , timeout = Nothing
        , tracker = Nothing
        }
```
Dieser Code erstellt einen HTTP-GET-Request mit Basisauthentifizierung. Bei Erfolg erhält man eine positive Antwort vom Server.
 
## Tiefere Einblicke

Die Basisauthentifizierung ist eine ältere Methode zur Authentifizierung von Webanfragen, wurde aber von neueren und sichereren Methoden wie dem OAuth 2.0 Protokoll weitgehend abgelöst.

Alternativen zur Basisauthentifizierung umfassen nicht nur OAuth, sondern auch Token-basierte Authentifizierungssysteme wie JWT (JSON Web Tokens), die oft in Single Page Applications (SPAs) eingesetzt werden.

Die Methode `basicAuth` benutzt die Funktion `Http.header` um einen "Authorization"-Header zu Ihrem HTTP-Request hinzuzufügen. Der Wert dieses Headers ist ein Basis-64-kodierter String, der aus Ihrem Benutzernamen, einem Doppelpunkt und Ihrem Passwort besteht.

## Siehe auch

Für weiterführende Informationen können folgende Links hilfreich sein:

- Elm Documentation zu HTTP-Requests: https://package.elm-lang.org/packages/elm/http/latest/
- Informationen zur Basisauthentifizierung: https://developer.mozilla.org/de/docs/Web/HTTP/Authentication
- Alternative Authentifizierungsmethoden: https://auth0.com/blog/cookies-vs-tokens-definitive-guide/