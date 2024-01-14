---
title:                "Swift: Das Herunterladen einer Webseite"
simple_title:         "Das Herunterladen einer Webseite"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt die Mühe machen, eine Webseite herunterzuladen? Nun, es gibt viele Gründe dafür! Zum Beispiel könnte man eine Kopie einer Webseite als Backup erstellen, um die Webseite offline zu betrachten oder um bestimmte Informationen zu extrahieren.

## Wie
Um eine Webseite in Swift herunterzuladen, gibt es einige Schritte zu beachten. Zunächst müssen Sie die URL der Webseite angeben, die Sie herunterladen möchten. Anschließend müssen Sie eine URLSession konfigurieren und eine URLRequest erstellen, die die URL enthält. Dann müssen Sie die Daten aus der Anfrage abrufen und sie in ein Data-Objekt speichern. Schließlich können Sie die Daten in einem beliebigen Format nutzen, z.B. um sie als String anzuzeigen oder sie in einer Datei zu speichern.

```Swift
// URL der Webseite
let url = URL(string: "https://www.example.com")!

// URLSession konfigurieren
let session = URLSession.shared

// URLRequest erstellen
let request = URLRequest(url: url)

// Daten abrufen und in ein Data-Objekt speichern
let task = session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Fehler: \(error)")
        return
    }
    guard let httpResponse = response as? HTTPURLResponse,
          (200...299).contains(httpResponse.statusCode) else {
        print("Serverfehler!")
        return
    }
    guard let mimeType = httpResponse.mimeType,
          mimeType == "text/html" else {
        print("Falscher MIME-Typ!")
        return
    }
    guard let data = data else {
        print("Keine Daten vorhanden!")
        return
    }
    // Daten in gewünschtes Format konvertieren
    let htmlString = String(data: data, encoding: .utf8)
    print(htmlString)
}
task.resume()
```

## Deep Dive
Ein tieferer Einblick in das Herunterladen von Webseiten umfasst auch die Verwendung von Closures und das Arbeiten mit den verschiedenen Antwortstatuscodes. Außerdem gibt es viele Optionen, um die URLSession weiter anzupassen, wie zum Beispiel das Festlegen von Timeout-Zeiten oder das Hinzufügen von HTTP-Headern.

Siehe auch:
- https://developer.apple.com/documentation/foundation/urlsession
- https://www.hackingwithswift.com/articles/195/advanced-techniques-for-experienced-swift-developers
- https://code.tutsplus.com/tutorials/working-with-scalajson-part-1--cms-30783