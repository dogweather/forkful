---
title:                "Eine http-Anfrage senden"
html_title:           "Swift: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP Anfragen zu senden ist ein wichtiger Teil des Web-Entwicklungsprozesses und ermöglicht es Programmierern, Daten von Servern abzurufen. Dies kann beispielsweise genutzt werden, um Inhalte dynamisch auf einer Webseite zu aktualisieren oder um mit APIs anderer Dienste zu kommunizieren.

## Wie geht's?
Um eine HTTP Anfrage in Swift zu senden, verwenden wir die `URLSession` und `URLRequest` Klassen. Zuerst erstellen wir eine `URLRequest` Instanz und definieren die URL, zu der wir eine Anfrage senden wollen. Dann erstellen wir eine `URLSession` und nutzen die `dataTask` Methode, um eine HTTP Anfrage durchzuführen und die Antwort in einem Closure zu verarbeiten. Zum Beispiel:

```Swift
let url = URL(string: "https://mywebsite.com/api/data")
var request = URLRequest(url: url)
request.httpMethod = "GET"
let session = URLSession.shared
let task = session.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
        return
    }
    // Verarbeite die Antwort
}
task.resume()
```

Die Antwort wird in diesem Beispiel in der `data` Variable gespeichert und kann dann weiter verarbeitet werden.

## Tiefer Einblick
HTTP (Hypertext Transfer Protocol) ist ein Protokoll, das für die Kommunikation zwischen Clients und Servern im Web verwendet wird. Es wurde 1991 entwickelt und ist seitdem das grundlegende Protokoll für den Austausch von Informationen im Internet. Eine Alternative zum Senden von HTTP Anfragen in Swift wäre die Verwendung von Drittanbieter-Frameworks wie Alamofire oder RestKit.

Die Implementierung von HTTP Anfragen in Swift basiert auf der `URLSession` API, die es uns ermöglicht, HTTP Anfragen zu senden und zu empfangen. Dabei können wir verschiedene HTTP Methoden wie GET, POST, PUT und DELETE verwenden, um unterschiedliche Aktionen auf dem Server auszuführen.

## Weitere Informationen
- [Apple Dokumentation zu URLSessions](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP auf Wikipedia](https://de.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- [Alamofire Framework](https://github.com/Alamofire/Alamofire)