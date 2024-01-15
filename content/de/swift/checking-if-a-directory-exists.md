---
title:                "Überprüfen, ob ein Verzeichnis exists."
html_title:           "Swift: Überprüfen, ob ein Verzeichnis exists."
simple_title:         "Überprüfen, ob ein Verzeichnis exists."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist oft notwendig, um sicherzustellen, dass Dateien oder Ordner korrekt gefunden und verwendet werden können. Es ist ein wichtiger Schritt in der Dateiverwaltung und kann Probleme im späteren Verlauf des Codes verhindern.

## Wie man das macht

Die Überprüfung, ob ein Verzeichnis existiert, kann in Swift auf verschiedene Arten durchgeführt werden. Eine Möglichkeit ist die Verwendung der `FileManager`-Klasse, die in der Standardbibliothek von Swift enthalten ist. Um dies zu demonstrieren, schauen wir uns ein einfaches Beispiel an, in dem wir überprüfen, ob ein Verzeichnis namens "Documents" unter dem Pfad "User/username" vorhanden ist.

```Swift
let fileManager = FileManager.default
let path = "/Users/username/Documents"

if fileManager.fileExists(atPath: path) {
    print("Das Verzeichnis existiert!")
} else {
    print("Das Verzeichnis existiert nicht.")
}
```

Die Ausgabe dieses Codes sollte "Das Verzeichnis existiert!" sein, wenn der angegebene Pfad tatsächlich existiert.

## Tiefere Einblicke

Es ist wichtig zu beachten, dass die Verwendung der `FileManager`-Klasse nur eine Möglichkeit ist, um die Existenz eines Verzeichnisses zu überprüfen. Es gibt auch andere Methoden, wie zum Beispiel die Verwendung von `URL` und `FileAttributeKey`. Darüber hinaus können Sie auch benutzerdefinierte Funktionen erstellen, um die Überprüfung und Verwaltung von Verzeichnissen in Ihrem Code zu vereinfachen.

Darüber hinaus gibt es in Swift einige wichtige Unterschiede in der Art, wie Verzeichnisse behandelt werden, im Vergleich zu anderen Programmiersprachen. Zum Beispiel sind Verzeichnisse in Swift eine Unterart von `URL`, während sie in den meisten anderen Sprachen als separate Entitäten behandelt werden.

## Siehe auch

Hier sind einige nützliche Ressourcen, um mehr über das Arbeiten mit Verzeichnissen in Swift zu erfahren:

- [Apple-Entwicklerdokumentation zur FileManager-Klasse (Englisch)](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift-Buch von Apple (Englisch)](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html)
- [Swift-Community-Forum (Englisch)](https://forums.swift.org/)