---
date: 2024-01-20 18:01:49.074131-07:00
description: "Wie geht das? Um eine HTTP-Anfrage mit Basisauthentifizierung in Haskell\
  \ zu senden, kann man die `http-client` und `base64-bytestring` Bibliotheken\u2026"
lastmod: '2024-03-13T22:44:53.931270-06:00'
model: gpt-4-1106-preview
summary: Um eine HTTP-Anfrage mit Basisauthentifizierung in Haskell zu senden, kann
  man die `http-client` und `base64-bytestring` Bibliotheken verwenden.
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

## Wie geht das?
Um eine HTTP-Anfrage mit Basisauthentifizierung in Haskell zu senden, kann man die `http-client` und `base64-bytestring` Bibliotheken verwenden. Hier ist ein einfaches Beispiel, wie das aussehen könnte:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack, append)

sendRequestWithBasicAuth :: String -> String -> String -> IO (Response ByteString)
sendRequestWithBasicAuth url username password = do
    manager <- newManager defaultManagerSettings
    let auth = Data.ByteString.Char8.pack $ username ++ ":" ++ password
        encodedAuth = encode auth
        headers = [(hAuthorization, Data.ByteString.Char8.append "Basic " encodedAuth)]
    initialRequest <- parseRequest url
    let request = initialRequest { method = "GET", requestHeaders = headers }
    response <- httpLbs request manager
    return response
```

Musteroutput: Wenn alles klappt, bekommst du die Antwort des Servers. Bei einem Fehler bekommst du eine entsprechende Fehlermeldung.

## Tiefgang
Historisch kommt die Basisauthentifizierung aus den frühen Tagen des Webs; es ist ein einfacher Mechanismus, der schon seit HTTP/1.0 existiert. Moderne Alternativen wie OAuth bieten stärkere Sicherheit und mehr Flexibilität. Die Implementierung in Haskell ist dank der vorhandenen Bibliotheken recht einfach, aber sie birgt das Risiko, dass unverschlüsselte Zugangsdaten abgefangen werden, falls sie nicht über HTTPS gesendet werden.

## Siehe auch
- Haskell `http-client` Bibliothek: http://hackage.haskell.org/package/http-client
- Haskell `base64-bytestring` Bibliothek: http://hackage.haskell.org/package/base64-bytestring
- HTTP Basic Authentication Spezifikation: https://tools.ietf.org/html/rfc7617
- Eine Erklärung modernerer Authentifizierungsmethoden: https://oauth.net/
