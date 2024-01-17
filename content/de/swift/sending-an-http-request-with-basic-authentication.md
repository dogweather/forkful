---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "Swift: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit Basisauthentifizierung ist ein Prozess, bei dem ein Programmierer einen Benutzernamen und ein Passwort verwendet, um auf eine geschützte Ressource im Internet zuzugreifen. Dies wird häufig verwendet, um sicherzustellen, dass nur autorisierte Benutzer auf bestimmte Inhalte oder Funktionalitäten zugreifen können.

## So geht's:

Um eine HTTP-Anfrage mit Basisauthentifizierung in Swift zu senden, können Sie die folgenden Schritte befolgen:

```
let username = "benutzername"
let password = "passwort"

// Erstellen Sie eine URL-Komponente mit der Basis-URL der Ressource
let url = URL(string: "https://beispiel.com/geschützte-ressource")!

// Erstellen Sie eine Anfrage mit dem URLRequest-Objekt und fügen Sie die Basisauthentifizierung hinzu
var request = URLRequest(url: url)
let loginString = "\(benutzername):\(passwort)"
let loginData = loginString.data(using: .utf8)
let base64LoginString = loginData?.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Senden Sie die Anfrage mit URLSession
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    // Verarbeiten Sie die Antwort und gegebenenfalls den Fehler
    if let error = error {
        print("Fehler beim Senden der Anfrage: \(error)")
        return
    }
    guard let data = data else {
        print("Keine Daten erhalten.")
        return
    }
    // Verarbeiten Sie die erhaltenen Daten
    print(String(data: data, encoding: .utf8)!)
}
task.resume()
```

Dies ist ein einfaches Beispiel, aber es zeigt, wie Sie die Basisauthentifizierung zu Ihrer Anfrage hinzufügen können.

## Tiefere Einblicke:

Basisauthentifizierung wurde in den späten 90er Jahren entwickelt und ist eine der ältesten Methoden zur Authentifizierung bei HTTP-Anfragen. Obwohl es weit verbreitet ist, hat es auch einige Nachteile, wie z.B. die Möglichkeit, dass Benutzername und Passwort im Klartext übertragen werden, was ein Sicherheitsrisiko darstellen kann. Alternativen zur Basisauthentifizierung sind z.B. OAuth oder API-Schlüssel.

Wenn Sie genauer verstehen möchten, wie die Basisauthentifizierung funktioniert, können Sie sich das HTTP-Header-Feld "Authorization" genauer ansehen. Hier werden Benutzername und Passwort im Base64-Format codiert und mit dem Präfix "Basic" versehen, um anzuzeigen, dass es sich um Basisauthentifizierung handelt.

## Siehe auch:

Für weitere Informationen und Beispiele zum Senden von HTTP-Anfragen in Swift können Sie unsere Artikel zu "HTTP-Anfragen in Swift" und "Verwendung von URLSession in Swift" lesen.