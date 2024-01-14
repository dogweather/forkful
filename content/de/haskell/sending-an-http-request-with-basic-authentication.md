---
title:                "Haskell: Ein HTTP-Anfrage mit Basisauthentifizierung senden."
simple_title:         "Ein HTTP-Anfrage mit Basisauthentifizierung senden."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung ist ein wichtiger Teil der Webentwicklung und ermöglicht den Zugriff auf geschützte Ressourcen oder Dienste. Diese Technik ist besonders nützlich, wenn vertrauliche Daten übertragen werden müssen oder um die Identität eines Benutzers zu überprüfen.

## Wie man es macht

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, können wir die standardmäßig in Haskell integrierte "Network.HTTP.Simple" Bibliothek verwenden. Wir müssen auch die "Data.ByteString" Bibliothek importieren, um auf die Authentifizierungsdaten zugreifen zu können.

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as Char8

url = "https://www.example.com/api"
username = "Benutzername"
password = "Passwort"

request = setRequestMethod "GET" $ setRequestBasicAuth (Char8.pack username) (Char8.pack password) $ parseRequest_ url
response = httpLBS request
```

Die obigen Codebeispiele zeigen eine beispielhafte HTTP-Anfrage an die URL "https://www.example.com/api" mit grundlegender Authentifizierung. Wir verwenden die "setRequestMethod" Funktion, um die Art der Anfrage festzulegen, in diesem Fall "GET". Dann nutzen wir die Funktion "setRequestBasicAuth" um den Benutzernamen und das Passwort einzugeben und schließlich wird die Anfrage mit "httpLBS" ausgeführt.

Die Antwort wird als "Response"-Datentyp zurückgegeben und kann je nach Anwendungsfall weiterverarbeitet werden. Eine typische Ausgabe sieht folgendermaßen aus:

```Haskell
Response {responseStatus = "200 OK", responseVersion = HTTP/1.1, responseHeaders = [...], responseBody = "{\"message\":\"Erfolgreiche Anfrage!\"}", responseCookieJar = ...}
```

## Tiefere Einblicke

Neben der grundlegenden Authentifizierung können auch andere Arten der Authentifizierung verwendet werden, wie z.B. die Digest-Authentifizierung. Die "Network.HTTP.Simple" Bibliothek unterstützt auch das Hinzufügen von benutzerdefinierten HTTP-Headern und das Parsen von JSON-Antworten.

Es ist auch wichtig zu beachten, dass die Übertragung von sensiblen Daten über eine unverschlüsselte Verbindung unsicher ist. Daher sollte immer HTTPS verwendet werden, wenn sensible Daten übertragen werden.

## Siehe auch

- [Haskell Network.HTTP.Simple Dokumentation](https://hackage.haskell.org/package/http-client)
- [Tutorial zum Senden von HTTP-Anfragen in Haskell](https://blog.logrocket.com/making-http-requests-in-haskell/)
- [Haskell Data.ByteString Dokumentation](https://hackage.haskell.org/package/bytestring)