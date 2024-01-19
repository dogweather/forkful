---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, Daten aus dem Internet zu extrahieren und auf deinem Gerät zu speichern. Warum machen Programmierer das? Um Informationen zu sammeln, Content zu analysieren oder um Offline-Zugriff zu ermöglichen.

## So geht's:

In Swift könntest du URLSession verwenden, um eine Webseite herunterladen. Hier ist ein einfacher Codeausschnitt:

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!

let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let error = error {
        print ("Fehler: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Daten:\n\(str!)")
    }
}
task.resume()
```

Führe das aus und du wirst HTML-Daten im Konsolenlog sehen.

## Detailbetrachtung:

Das Extrahieren von Webseiten ist keine neue Praxis. Es entstand mit dem Wachstum des Internets und des Informationstauschs. Historisch gesehen wurde dies oft mit Sprachen wie Perl und Bibliotheken wie LWP durchgeführt.

Es gibt alternativen zu URLSession, wie z.B. Alamofire, eine Swift-basierte HTTP-Netzwerkbibliothek. Alamofire kann in vielerlei Hinsicht nützlich sein, einschließlich der Unterstützung für mehrere Datenformate und Network Reachability.

Was die Implementierung betrifft, so erledigt URLSession unter der Haube die meisten Aufgaben. Du brauchst nur die Methode dataTask(with:completionHandler:) aufrufen, die dann die Daten für dich holt.

## Siehe auch:

Für eine Vertiefung deines Wissens hier einige hilfreiche Quellen:

1. Apple's URLSession - https://developer.apple.com/documentation/foundation/urlsession
2. Alamofire auf GitHub - https://github.com/Alamofire/Alamofire
3. Web-Sraping Leitfaden - https://www.datacamp.com/community/tutorials/web-scraping-using-python