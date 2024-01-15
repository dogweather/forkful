---
title:                "Webseite herunterladen"
html_title:           "Swift: Webseite herunterladen"
simple_title:         "Webseite herunterladen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Hast du schon mal eine Webseite heruntergeladen? Wenn nicht, wäre es eine tolle Möglichkeit, deine Fähigkeiten als Swift-Programmierer zu verbessern und gleichzeitig etwas Nützliches zu tun. Mit Swift können wir ganz einfach Webseiten herunterladen und sie lokal auf unseren Geräten speichern. In diesem Artikel zeige ich dir, wie das geht und gebe dir einen Einblick in die Funktionsweise.

## Wie geht's?

Um eine Webseite herunterzuladen, müssen wir zuerst die URL angeben, von der wir sie herunterladen möchten. Zum Beispiel:

```Swift
let url = "https://www.example.com"
```

Dann verwenden wir die URL-Klasse, um eine Instanz zu erstellen, die auf diese URL verweist:

```Swift
if let url = URL(string: url) {
  // do something
}
```

Als nächstes verwenden wir die URLSession-Klasse, um eine Verbindung zu öffnen und die Daten von der URL abzurufen. Dazu müssen wir auch eine URLSessionDataTask erstellen, die uns die heruntergeladenen Daten zurückgibt:

```Swift
let session = URLSession.shared
let task = session.dataTask(with: url) { (data, response, error) in
  // handle response
}
task.resume()
```

Im completionHandler können wir nun die empfangenen Daten verarbeiten und auf unsere lokale Festplatte speichern:

```Swift
if let data = data {
  // save data to local file
}
```

Das ist alles, was wir brauchen, um eine Webseite herunterzuladen und lokal zu speichern. Natürlich können wir auch zusätzliche Optionen und Einstellungen hinzufügen, je nachdem was wir mit den heruntergeladenen Daten machen möchten.

## Deep Dive

Wenn wir uns die Funktionen der URLSession und URLSessionDataTask genauer ansehen, können wir sehen, dass sie auf der unterliegenden Netzwerkschicht des Betriebssystems aufbauen. Dadurch können wir effizient und sicher Daten aus dem Internet herunterladen. Die URLSession-Klasse bietet auch verschiedene Konfigurationsoptionen, z.B. können wir den Timeout oder die Anzahl der gleichzeitigen Verbindungen angeben.

## Siehe auch

- [Apple Developer Documentation for URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Swift By Sundell: Downloading and caching images](https://www.swiftbysundell.com/posts/downloading-and-caching-images-in-swift)
- [Hacking with Swift: How to download files with URLSession and downloadTask](https://www.hackingwithswift.com/example-code/networking/how-to-download-files-with-nsurlsession-datadownloadtask)