---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit Basic Authentication ist ein Prozess, bei dem Benutzername und Passwort in der Anfrageheader mitgesendet werden, um eine sichere Verbindung zu den Ressourcen eines Servers herzustellen. Programmierer nutzen es, um Datenzugriff nur für authentifizierte Benutzer zu gewährleisten und dabei eine einfache, standardisierte Methode zur Authentifizierung zu verwenden. 

## So geht's:

Sie benötigen das Paket `http-conduit`. Stellen Sie sicher, dass es installiert ist.
```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client (applyBasicAuth)

let request = setRequestHost "your-host.com" $ 
              setRequestPort 80 $ 
              setRequestMethod "GET" $ 
              setRequestPath "/your-path" $ 
              setRequestSecure False $ 
              defaultRequest
              
let requestWithAuth = applyBasicAuth "username" "password" request

response <- httpLBS requestWithAuth
```
Die Antwort enthält den Statuscode und den Inhalt der Antwort.

## Deep Dive:

Die Basic Authentication ist eine seit langem etablierte Methode, die in der RFC 7617 spezifiziert ist. Allerdings ist sie einfach und bietet keinen Sicherheitsschutz wie moderne Alternativen wie OAuth. Bei der Verwendung von Basic Authentication ist die Verschlüsselung mit HTTPS sehr wichtig.

Alternativen können OAuth und OAuth2 sein, jedes empfohlen für unterschiedliche Szenarien.

Die Implementierung in Haskell macht Gebrauch von monadischen Aspekten, um den typisch funktionalen Stil aufrechtzuerhalten und gleichzeitig die Möglichkeit zu bieten, Zustände, wie die Antwort auf eine Anfrage, zu speichern.

## Dazu passende Themen:

- Weiterführende Informationen zu HTTP-Anfragen in Haskell gibt es hier: [Http Conduit in Haskell](https://hackage.haskell.org/package/http-conduit)
- Für eine tiefere Analyse von Zuständen in Haskell empfehlen wir: [State Monad](https://wiki.haskell.org/State_Monad)
- Weitere Informationen zur Basic Authentication: [Basic Authentication](https://tools.ietf.org/html/rfc7617)