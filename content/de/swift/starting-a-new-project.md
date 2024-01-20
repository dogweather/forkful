---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Starten eines neuen Projekts ist der erste Schritt zur Entwicklung von Anwendungen. Es ist essenziell, um die Arbeit zu organisieren und Themen wie die Architektur, die Logik und sogar das Design Ihrer App zu definieren.

## So geht's: 
Ein neues Swift-Projekt ist einfach zu starten. Zunächst öffnet man Xcode und wählt "Create a New Xcode Project". Für eine einfache iPhone-App wählt man "App" und füllt das Projektinformationen Formular aus.

```Swift
// HelloWorld.swift
import SwiftUI

struct HelloWorld: View {
    var body: some View {
        Text("Hallo, Welt!")
    }
}

struct HelloWorld_Previews: PreviewProvider {
    static var previews: some View {
        HelloWorld()
    }
}
```
Dies ist der Code für eine einfache SwiftUI-Anwendung, die "Hallo, Welt!" auf dem Bildschirm anzeigt.

## Vertiefende Informationen: 
Das Öffnen eines neuen Projekts in Swift bietet eine saubere Arbeitsumgebung, die darauf ausgerichtet ist, Ihr Projekt kompakt und gut organisiert zu halten. Historisch gesehen wurde Swift 2014 von Apple entwickelt, das Konzept des "Projekts" war jedoch bereits vor Swift in der Programmierung bekannt. Alternative Ansätze könnten die Verwendung von Modellen und Frameworks Dritter beinhalten, die auf den Projekttyp abgestimmt sind (Beispiel: Spieleentwicklung mit Unity oder Website-Baukästen für Landing Pages). Bei der Erstellung eines neuen Projekts sollten Sie überlegen, welche Dateien und Bibliotheken benötigt werden, und diese in das Projekt einbeziehen, um den Ablauf und die Organisation zu verbessern.

## Siehe auch: 
• [Apple Developer Documentation - Create a new project](https://developer.apple.com/documentation/xcode/creating_a_new_project)
• [Swift Basics - Start a new Project](https://www.swiftbysundell.com/basics/starting-a-new-project/)
• [SwiftUI - Create a new project](https://www.hackingwithswift.com/quick-start/swiftui/how-to-create-a-new-project)