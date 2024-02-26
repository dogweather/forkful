---
date: 2024-01-20 18:02:54.481387-07:00
description: "Beim Senden einer HTTP-Anfrage mit Basisauthentifizierung f\xFCgen wir\
  \ Anmeldeinformationen in den Request-Header ein, um uns beim Server zu\u2026"
lastmod: '2024-02-25T18:49:51.275919-07:00'
model: gpt-4-1106-preview
summary: "Beim Senden einer HTTP-Anfrage mit Basisauthentifizierung f\xFCgen wir Anmeldeinformationen\
  \ in den Request-Header ein, um uns beim Server zu\u2026"
title: HTTP-Anfragen mit Basisauthentifizierung senden
---

{{< edit_this_page >}}

## Was & Warum?
Beim Senden einer HTTP-Anfrage mit Basisauthentifizierung fügen wir Anmeldeinformationen in den Request-Header ein, um uns beim Server zu authentifizieren. Programmierer nutzen dies, um sicherzustellen, dass nur autorisierte Benutzer auf geschützte Ressourcen zugreifen können.

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
