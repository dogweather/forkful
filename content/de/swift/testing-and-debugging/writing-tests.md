---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:02.176373-07:00
description: "Das Schreiben von Tests in Swift umfasst das Erstellen und Ausf\xFC\
  hren von Code, der die Korrektheit anderer Codeeinheiten in Ihrer Anwendung \xFC\
  berpr\xFCft.\u2026"
lastmod: '2024-03-13T22:44:54.229016-06:00'
model: gpt-4-0125-preview
summary: "Das Schreiben von Tests in Swift umfasst das Erstellen und Ausf\xFChren\
  \ von Code, der die Korrektheit anderer Codeeinheiten in Ihrer Anwendung \xFCberpr\xFC\
  ft.\u2026"
title: Tests Schreiben
weight: 36
---

## Was & Warum?
Das Schreiben von Tests in Swift umfasst das Erstellen und Ausführen von Code, der die Korrektheit anderer Codeeinheiten in Ihrer Anwendung überprüft. Programmierer tun dies, um Zuverlässigkeit zu gewährleisten, Fehler früh im Entwicklungszyklus zu erkennen und zukünftiges Code-Refactoring ohne unbeabsichtigte Konsequenzen zu erleichtern.

## Wie geht das:
Swift unterstützt das Testen durch sein XCTest-Framework, das in Xcode integriert ist. Sie können Unit-Tests schreiben, um einzelne Teile Ihres Codes zu überprüfen, zum Beispiel eine Funktion, die die Summe von zwei Zahlen berechnet.

```swift
import XCTest
@testable import IhreApp

class IhreAppTests: XCTestCase {

    func testSumme() {
        let ergebnis = Rechner().summe(a: 1, b: 2)
        XCTAssertEqual(ergebnis, 3, "Die Summenfunktion hat nicht den erwarteten Wert zurückgegeben.")
    }
}
```

Um diesen Test auszuführen, würden Sie typischerweise Command-U in Xcode drücken. Die Ausgabe im Xcode-Testnavigator zeigt Ihnen, ob der Test bestanden oder fehlgeschlagen ist.

Zum Beispiel eine erfolgreiche Testausgabe:
```
Testfall '-[IhreAppTests testSumme]' erfolgreich (0.005 Sekunden).
```

Für fortgeschrittenere Testszenarien könnten Sie Drittanbieter-Bibliotheken wie Quick/Nimble adoptieren, die eine expressivere Syntax für das Schreiben von Tests bieten.

Mit Quick/Nimble könnten Sie denselben Test so schreiben:

```swift
// Fügen Sie Quick und Nimble zu Ihrem Swift-Paketmanager hinzu oder verwenden Sie CocoaPods/Carthage, um sie zu installieren
import Quick
import Nimble
@testable import IhreApp

class RechnerSpec: QuickSpec {
    override func spec() {
        beschreibe("Rechner") {
            kontext("beim Addieren von Zahlen") {
                es("sollte die korrekte Summe zurückgeben") {
                    let rechner = Rechner()
                    expect(rechner.summe(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Die Ausführung dieses Tests würde Ihnen eine ähnliche Ausgabe in Ihrer Testkonsole oder im Protokoll des CI/CD-Tools liefern, die anzeigt, ob der Test erfolgreich war oder fehlgeschlagen ist, mit einem lesbareren Format für die Beschreibung von Tests und Erwartungen.
