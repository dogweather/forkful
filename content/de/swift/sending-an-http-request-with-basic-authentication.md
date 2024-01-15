---
title:                "Versenden einer HTTP-Anfrage mit einfacher Authentifizierung"
html_title:           "Swift: Versenden einer HTTP-Anfrage mit einfacher Authentifizierung"
simple_title:         "Versenden einer HTTP-Anfrage mit einfacher Authentifizierung"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum

Das Versenden von HTTP-Anfragen mit grundlegender Authentifizierung ist eine gängige Methode, um sicherzustellen, dass nur autorisierte Benutzer auf bestimmte Ressourcen zugreifen können. Dies ist besonders nützlich für Web- oder Mobilanwendungen, die eine sichere Kommunikation mit einem Server erfordern.

# Wie es geht

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Swift zu senden, müssen Sie zuerst eine URL-Session erstellen und eine URL für den Server angeben, an den Sie die Anfrage senden möchten. Anschließend können Sie einer URLRequest das Authentifizierungsheader-Feld hinzufügen, das die Basis64-Codierung von Benutzername und Passwort enthält. Hier ist ein Beispielcode, der eine Anfrage an die GitHub-API mit grundlegender Authentifizierung sendet:

```Swift
let url = URL(string: "https://api.github.com/users/octocat/repos")!

let session = URLSession.shared

// Erstelle eine URLRequest mit der URL und der Anfrageart angeben
var request = URLRequest(url: url)
request.httpMethod = "GET"

// Hinzufügen des Authentifizierungsheaders mit Basis64-Codierung
let username = "username"
let password = "password"
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Senden der Anfrage
let task = session.dataTask(with: request) { (data, response, error) in
    // Verarbeiten der Antwort
    if let error = error {
        print("Fehler: \(error)")
    } else if let data = data {
        print("Daten empfangen: \(data)")
    }
}
task.resume()
```

Die obige Anfrage sendet eine HTTP-GET-Anfrage an die GitHub-API, um eine Liste der Repositories für den Benutzer "octocat" abzurufen. Beachten Sie, dass der Username und das Passwort durch Ihre eigenen Anmeldedaten ersetzt werden müssen.

# Tiefer Einblick

Die grundlegende Authentifizierung ist eine einfache Methode zur Authentifizierung von Benutzern, die jedoch nicht die sicherste Option ist, da die Benutzeranmeldedaten als Teil der Anfrage übertragen werden. Es ist daher wichtig, dass die Verbindung zum Server über HTTPS erfolgt, um die Kommunikation zu verschlüsseln.

Eine Möglichkeit, die Sicherheit der grundlegenden Authentifizierung zu verbessern, ist die Verwendung von OAuth oder anderen Token-basierten Authentifizierungsmethoden, bei denen ein temporärer Token anstelle von Anmeldedaten verwendet wird. Dies macht es schwieriger für Angreifer, Zugriff auf die Konten von Benutzern zu erlangen.

# Siehe auch

- [Apple Dokumentation zu URL-Session](https://developer.apple.com/documentation/foundation/urlsession)
- [GitHub API-Dokumentation für die Authentifizierung](https://developer.github.com/v3/auth/)
- [OAuth-Authentifizierung erklärt](https://oauth.net/2/)