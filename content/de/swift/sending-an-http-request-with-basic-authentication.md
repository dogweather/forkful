---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basisauthentifizierung erlauben es Programmierern, Daten von einem Server zu holen, was durch Benutzername und Passwort gesichert ist. Sie sind die Notwendigkeit, wenn du auf geschützte Informationen zugreifen musst und dabei den Server dafür authentifizieren willst, dass du der bist, wer sagt, dass du bist.

## So geht's:
Sie können dies in Swift erreichen durch:

```Swift
import Foundation

let user = "username"
let password = "password"

let url = URL(string: "http://example.com")!
var request = URLRequest(url: url)
let loginString = "\(user):\(password)"
if let data = loginString.data(using: .utf8) {
    let base64 = data.base64EncodedString()
    request.setValue("Basic \(base64)", forHTTPHeaderField: "Authorization")
}

let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    // Handle response here
}
task.resume()
```
Wenn Sie dieses Programm ausführen, sendet es eine Anfrage an `http://example.com` mit Basissicherheit (`username`/`password`).

## Tiefere Info:
Historisch gesehen sind HTTP-Anfragen mit Basisauthentifizierung für die Übertragung von sensiblen Daten nicht ausreichend, da die Anmeldeinformationen im Klartext gesendet werden. Deshalb sollten sie über sichere Verbindungen (HTTPS) gemacht werden.

Eine prominenteste Alternative wäre die Verwendung eines OAuth2-Tokens für die Authentifizierung. Es bietet mehr Sicherheit und Kontrolle, erfordert jedoch eine komplexere Einrichtung.

In der Umsetzung sind Details zu beachten. Der "Authorization"-Header enthält die Base64-kodierten Anmeldeinformationen im Format "Benutzername:Passwort". Beachte, dass Base64 unverschlüsselt ist, was die Notwendigkeit einer sicheren Verbindung unterstreicht.

## Siehe auch:
HTTP-Anfragen in Swift: https://developer.apple.com/documentation/foundation/url_loading_system/making_http_and_https_requests

Basis-Authentifizierung: https://tools.ietf.org/html/rfc7617

Infos zur sichereren OAuth2-Authentifizierung: https://oauth.net/2/

Details zur Base64-Kodierung: https://developer.apple.com/documentation/foundation/data/1417069-base64encodedstring