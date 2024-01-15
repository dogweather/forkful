---
title:                "Versenden einer http Anfrage mit grundlegender Authentifizierung"
html_title:           "Haskell: Versenden einer http Anfrage mit grundlegender Authentifizierung"
simple_title:         "Versenden einer http Anfrage mit grundlegender Authentifizierung"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich mit dem Versenden eines HTTP-Requests mit grundlegender Authentifizierung auseinandersetzen? Nun, wenn du eine Anwendung entwickelst, die auf einen Webdienst zugreifen muss, kann es sein, dass dieser eine grundlegende Authentifizierung erfordert. In diesem Fall ist es wichtig zu verstehen, wie man eine solche Anfrage sendet, damit deine Anwendung reibungslos läuft.

## How To
Hier ist ein Beispiel, wie du eine HTTP-Anfrage mithilfe von `basicAuth` aus der `Network.HTTP.Simple`-Bibliothek in Haskell senden kannst:

```Haskell
import Network.HTTP.Simple (httpLbs, parseRequest, setRequestMethod, setRequestBasicAuth, getResponseBody)

main = do
    request <- parseRequest "https://www.example.com"
    let authRequest = setRequestMethod "GET" $ setRequestBasicAuth "username" "password" request
    response <- httpLbs authRequest
    body <- getResponseBody response
    print body
```

Das `basicAuth`-Funktion verlangt zwei Argumente - einen Benutzernamen und ein Passwort - und gibt eine neue `Request`-Instanz zurück, die für die Authentifizierung vorbereitet ist. Hier wird die Anfrage mit dem `GET`-Methode gesendet und die Antwort wird in einer `String`-Variablen namens `body` gespeichert und ausgegeben.

## Deep Dive
Wenn du dich genauer mit dem Senden von HTTP-Anfragen mit grundlegender Authentifizierung beschäftigen möchtest, kannst du die Dokumentation der `basicAuth`-Funktion in der `Network.HTTP.Simple`-Bibliothek überprüfen. Dort findest du weitere Details und Optionen, wie zum Beispiel die Verwendung von Custom-Schemas für die Authentifizierung.

## Siehe auch
- https://hackage.haskell.org/package/http-client-0.7.1/docs/Network-HTTP-Client.html#v:basicAuth
- https://hackage.haskell.org/package/http-client-tls-0.3.5/docs/Network-HTTP-Client-TLS.html