---
title:                "Swift: Eine http-Anfrage mit grundlegenden Authentifizierung senden."
simple_title:         "Eine http-Anfrage mit grundlegenden Authentifizierung senden."
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum
Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung ist ein wichtiger Teil der Programmierung in Swift, insbesondere bei der Interaktion mit APIs oder anderen Back-End-Systemen. Durch die Bereitstellung von Benutzername und Passwort können Sie sicherstellen, dass Ihre Anfragen nur von autorisierten Benutzern verwendet werden können.

## So geht's
Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Swift zu senden, müssen Sie zuerst eine Instanz von `URLSession` erstellen. Dann können Sie die `URLSessionDataTask`-Klasse verwenden, um Ihre Anfrage mit einem `URLRequest`-Objekt zu erstellen. Hier ist ein Beispielcode:

```Swift
// Benutzername und Passwort
let username = "meinBenutzername"
let password = "meinPasswort"

// URL der Anfrage
let url = URL(string: "https://www.meine-api.com")

// Erstellen der Anfrage mit grundlegender Authentifizierung
var request = URLRequest(url: url!)
request.httpMethod = "GET"

// Hinzufügen von Benutzername und Passwort zur Anfrage
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Sendet die Anfrage mit SessionDataTask
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {                                                 
        print(error?.localizedDescription ?? "Fehler beim Abrufen")
        return
    }                                                                                         
    print(String(data: data, encoding: .utf8)!)                                              
}                                                                                              
task.resume() 
```

Dieses Beispiel zeigt, wie Sie die Anfrage mit Ihrem Benutzernamen und Passwort authentifizieren und die Antwort durch die `URLSessionDataTask`-Klasse abrufen können.

## Tiefergehende Erklärung
Die grundlegende Authentifizierung basiert auf dem HTTP-Header "Authorization", der den Benutzernamen und das Passwort in einem Base64 codierten String enthält. Bei der Erstellung des `URLRequest`-Objekts fügen wir diesen Header hinzu, um unsere Anfrage zu authentifizieren. Wichtig ist auch, dass der Benutzername und das Passwort in einem separaten `Data`-Objekt gespeichert werden, bevor sie Base64-kodiert werden. 

Ein häufiger Fehler beim Senden von HTTP-Anfragen mit grundlegender Authentifizierung ist der Versuch, das Benutzername-Passwort-Paar direkt in den HTTP-Header zu schreiben, anstatt es Base64-kodiert zu übergeben. Dies kann dazu führen, dass die Anfrage abgelehnt wird.

## Weitere Artikel
Für weitere Informationen und Beispiele zur Verwendung von HTTP-Anfragen in Swift empfehle ich die folgenden Links:

- [Apple Entwickler Doku - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [How to use URLSession](https://useyourloaf.com/blog/how-to-use-URLSession-with-swift/)
- [Swift by Sundell - Basics of Network Requests](https://www.swiftbysundell.com/basics/network-requests/) 

## Siehe auch
- [Wie man Swift Package Manager verwendet](https://www.meine-blog.com/Swift-Package-Manager)
- [Einführung in die mobile App-Entwicklung mit Swift](https://www.meine-blog.com/Swift-App-Entwicklung)
- [Grundlagen der Swift Programmierung: Variablen und Konstanten](https://www.meine-blog.com/Swift-Programmierung)