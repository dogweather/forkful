---
title:                "Herunterladen einer Webseite"
html_title:           "Swift: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was ist das und Warum?
Das Herunterladen von Webseiten kann als das Abrufen von Inhalten von einer Internetquelle betrachtet werden. Programmierer tun das, um diese Inhalte in ihre Anwendungen einzubinden oder zu verarbeiten.

## Wie geht's?
Um eine Webseite herunterzuladen, können Sie die folgende Methode verwenden:
```Swift
let url = URL(string: "https://www.example.com")
let task = URLSession.shared.dataTask(with: url!) { (data, response, error) in
    guard let data = data, let content = String(data: data, encoding: .utf8) else { return }
    print(content)
}
task.resume()
```

Das obige Beispiel zeigt, wie man mithilfe von `URLSession` eine Datenanfrage ausführt und die Antwort in Form von Daten erhält. Diese Daten können dann zur weiteren Verarbeitung oder Anzeige verwendet werden.

## Tief tauchen
Das Herunterladen von Webseiten hat eine lange Geschichte in der Programmierung. In den Anfängen des Internets geschah dies mithilfe von Bibliotheken wie `wget`, die die Verarbeitung von URLs und das Herunterladen von Dateien ermöglichten. Heutzutage gibt es jedoch APIs wie `URLSession`, die dies wesentlich einfacher machen.

Es gibt auch Alternativen zum Herunterladen von Webseiten, wie z.B. das Abrufen von Informationen von APIs oder das Parsen von HTML-Dateien. Letztendlich hängt die Wahl vom konkreten Anwendungsfall ab.

## Sieh auch
Weitere Informationen und Beispiele zu diesem Thema finden Sie in der offiziellen Dokumentation von Swift: https://docs.swift.org/swift-book/LanguageGuide/URLSessions.html