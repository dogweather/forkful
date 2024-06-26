---
date: 2024-01-20 18:02:54.481387-07:00
description: 'How to: Mit Swift eine HTTP-Anfrage mit Basisauthentifizierung senden.'
lastmod: '2024-03-13T22:44:54.225417-06:00'
model: gpt-4-1106-preview
summary: Mit Swift eine HTTP-Anfrage mit Basisauthentifizierung senden.
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

## How to:
Mit Swift eine HTTP-Anfrage mit Basisauthentifizierung senden:

```Swift
import Foundation

let username = "benutzer"
let password = "passwort"
let loginString = "\(username):\(password)"
if let loginData = loginString.data(using: .utf8) {
    let base64LoginString = loginData.base64EncodedString()

    var request = URLRequest(url: URL(string: "https://beispielapi.de/daten")!)
    request.httpMethod = "GET"
    request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

    let session = URLSession.shared
    session.dataTask(with: request) { data, response, error in
        guard let data = data, error == nil else { print(error?localizedDescription ?? "Unbekannter Fehler"); return }
        if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 {
            // Verarbeite die Antwort
            print(String(data: data, encoding: .utf8) ?? "Antwort konnte nicht verarbeitet werden")
        } else {
            print("HTTP Status Code: \((response as? HTTPURLResponse)?.statusCode ?? -1)")
        }
    }.resume()
}
```

Die Ausgabe wäre die Antwort des Servers oder ein Fehler, falls etwas schiefgeht.

## Deep Dive
Basisauthentifizierung ist eine Methode von HTTP, bei der Benutzername und Passwort kodiert und im `Authorization`-Header gesendet werden. Das ist nicht die sicherste Authentifizierungsmethode, da die Anmeldeinformationen im Klartext kodiert sein könnten, wenn sie nicht über HTTPS gesendet werden. Moderne Alternativen wie OAuth oder Token-basierte Authentifizierung bieten mehr Sicherheit und sind oft besser geeignet. Apple's `URLSession` macht es leicht, solche Anfragen zu implementieren, aber es ist wichtig, über zusätzliche Sicherheitsaspekte wie das Speichern von Anmeldeinformationen nachzudenken.

## See Also
- [Apple's URLSession Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Basic Access Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Secure Coding Guide von Apple](https://developer.apple.com/documentation/security)
