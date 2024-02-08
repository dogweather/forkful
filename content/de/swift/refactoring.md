---
title:                "Refactoring"
aliases:
- de/swift/refactoring.md
date:                  2024-01-26T03:36:37.835050-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist der Prozess des Umstrukturierens vorhandenen Computer-Codes, ohne dessen externes Verhalten zu ändern. Programmierer machen dies, um den Code zu säubern, die Lesbarkeit und Wartbarkeit zu verbessern und den Weg für zukünftige Funktionen mit minimaler technischer Schuld zu ebnen.

## Wie geht das:
Beginnen wir mit einem einfachen Swift-Beispiel, in dem wir etwas wiederholenden Code haben:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Vorname: \(firstName)")
    print("Nachname: \(lastName)")
    print("Alter: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Jobtitel: \(title)")
    print("Firma: \(company)")
}
```

Ein Refactoring hierfür würde beinhalten, eine `User`-Struktur zu erstellen, um Benutzerattribute zu kapseln und eine Methode zum Ausdrucken der Details hinzuzufügen:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Vorname: \(firstName)")
        print("Nachname: \(lastName)")
        print("Alter: \(age)")
        print("Jobtitel: \(jobTitle)")
        print("Firma: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Softwareentwickler", company: "Tech Solutions")
user.printDetails()
```

### Beispiel-Ausgabe:
```
Vorname: John
Nachname: Doe
Alter: 30
Jobtitel: Softwareentwickler
Firma: Tech Solutions
```

## Vertiefung
Refactoring hat Wurzeln, die bis in die frühen Tage der Softwaretechnik zurückgehen, aber der Begriff wurde Ende der 1990er Jahre popularisiert, insbesondere durch Martin Fowlers bahnbrechendes Buch "Refactoring: Improving the Design of Existing Code". Das Buch legte das Prinzip dar, dass Code kontinuierlich in kleinen Schritten aufgeräumt werden sollte, anstatt auf eine separate Phase zu warten.

Alternativen zum manuellen Refactoring umfassen automatisierte Tools und IDEs (Integrierte Entwicklungsumgebungen), die beim Erkennen von doppeltem Code helfen, Vereinfachungen vorschlagen und Teile des Codes automatisch generieren können. Xcode, für Swift-Entwicklung, bietet verschiedene Refactoring-Tools, wie die Umbenennen- und Methode-extrahieren-Funktionalität, die das Potenzial für menschliche Fehler im Prozess reduzieren können.

Beim Implementieren von Refactoring ist es wichtig, eine solide Testsuite zu haben. Tests dienen als Sicherheitsnetz und stellen sicher, dass die Änderungen, die Sie vornehmen, keine Fehler einführen. Dies ist entscheidend, da das Hauptziel des Refactoring darin besteht, die interne Struktur zu ändern, ohne das externe Verhalten zu beeinflussen.

## Siehe auch
- ["Refactoring: Improving the Design of Existing Code" von Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [Swift-Dokumentation von Apple](https://swift.org/documentation/)
- [Verwendung der Refactoring-Tools von Xcode](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Swift Style Guide von Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
