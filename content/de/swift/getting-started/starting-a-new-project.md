---
date: 2024-01-20 18:04:38.541939-07:00
description: "Ein neues Projekt zu starten bedeutet, eine leere Leinwand in eine funktionierende\
  \ App umzuwandeln. Entwickler machen das, um Ideen zum Leben zu erwecken\u2026"
lastmod: '2024-03-13T22:44:54.226322-06:00'
model: gpt-4-1106-preview
summary: Ein neues Projekt zu starten bedeutet, eine leere Leinwand in eine funktionierende
  App umzuwandeln.
title: Einen neuen Projekt starten
weight: 1
---

## What & Why? (Was & Warum?)
Ein neues Projekt zu starten bedeutet, eine leere Leinwand in eine funktionierende App umzuwandeln. Entwickler machen das, um Ideen zum Leben zu erwecken und Probleme mit maßgeschneiderten Lösungen zu lösen.

## How to: (Wie geht das:)
```Swift
// Erstellen eines neuen Xcode-Projekts
// 1. Xcode starten
// 2. "Create a new Xcode project" (Ein neues Xcode-Projekt erstellen) wählen
// 3. Template auswählen, z.B. "Single View App"
// 4. Projektinformationen eingeben:
//    - Projektname: HelloWorld
//    - Team: Falls relevant
//    - Organisation Name: DeinName oder Firma
//    - Organisation Identifier: z.B. com.deinname
//    - Sprache: Swift
//    - Häkchen bei "Use Core Data" für Datenpersistenz, falls nötig
// 5. Speicherort wählen und Projekt erstellen

// Hello World in Swift
import UIKit

class ViewController: UIViewController {
    override func viewDidLoad() {
        super.viewDidLoad()
        
        // Eine Label-Komponente erstellen
        let label = UILabel(frame: CGRect(x: 20, y: 50, width: 200, height: 20))
        label.text = "Hallo Welt"
        
        // Label zum View hinzufügen
        self.view.addSubview(label)
    }
}

// In der Konsole sollte "Build succeeded" angezeigt werden, wenn keine Fehler vorliegen.
```

## Deep Dive (Tiefer eintauchen)
Die Tradition, mit "Hello World" zu starten, kommt aus dem 1978 erschienenen Buch "The C Programming Language". Es ist ein einfacher Weg, das Setup zu testen und ein erstes Erfolgserlebnis zu haben. Alternativ zu Xcode können SwiftPM oder Swift Playgrounds verwendet werden, besonders bei kleineren Projekten oder beim Lernen. Während Xcode umfangreich und für macOS-Entwicklung optimiert ist, bietet SwiftPM einen leichtgewichtigeren Ansatz und ist ideal für Backend-Projekte. Swift Playgrounds wiederum ist großartig für schnelles Prototyping und Lernen auf iPad und Mac.

## See Also (Siehe auch)
- [Swift.org - Getting Started](https://www.swift.org/getting-started/)
- [Apple Developer - Xcode](https://developer.apple.com/xcode/)
- [Swift Package Manager](https://swift.org/package-manager/)
