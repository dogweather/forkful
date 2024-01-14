---
title:    "Swift: Umwandeln eines Datums in einen String"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Warum
Das Konvertieren eines Datums in einen String ist eine wichtige Aufgabe in der Swift-Programmierung, insbesondere wenn es darum geht, Daten in einer benutzerfreundlichen Form anzuzeigen. Dieser Prozess ermöglicht es Ihnen, Datumsangaben in einem bestimmten Format zu präsentieren, das für den Benutzer leicht lesbar ist.

# So geht's
Um ein Datum in einen String umzuwandeln, können Sie die `DateFormatter`-Klasse in Verbindung mit dem `dateFormat`-Eigenschaft verwenden. Hier ist ein Beispiel, wie Sie das aktuelle Datum in das Format "dd.MM.yyyy" konvertieren können:
```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let dateString = formatter.string(from: date)
print(dateString)
// Ausgabe: 08.07.2021
```
In diesem Beispiel erstellen wir zunächst ein neues `Date`-Objekt mit dem aktuellen Datum. Dann initialisieren wir einen `DateFormatter` und legen das gewünschte Datumsformat fest. Schließlich rufen wir die `string(from:)`-Methode auf, um das Datum in einen String umzuwandeln. Die Ausgabe wird je nach aktuellem Datum variieren.

# Tiefer Einblick
Beim Konvertieren von Datumswerten in Strings gibt es einige wichtige Dinge zu beachten. Zum einen müssen Sie das gewünschte Datumformat in die `dateFormat`-Eigenschaft einfügen, wie im obigen Beispiel gezeigt. Die verschiedenen Formatierungszeichen, die Sie dabei verwenden können, werden in der Dokumentation von Apple ausführlich erklärt.

Darüber hinaus kann es auch hilfreich sein, die Zeitzone und den Kalender in Betracht zu ziehen, wenn Sie mit Datumsangaben arbeiten. Sie können diese über die entsprechenden Eigenschaften des `DateFormatter`-Objekts einstellen.

Schließlich bietet die `DateFormatter`-Klasse noch viele weitere Funktionen und Methoden, mit denen Sie noch umfangreichere Konvertierungen durchführen können, wie beispielsweise das Extrahieren von einzelnen Datumskomponenten oder das Konvertieren von Zeichenfolgen in ein `Date`-Objekt.

# Siehe auch
- [Apple Dokumentation zur DateFormatter-Klasse](https://developer.apple.com/documentation/foundation/dateformatter)
- [Weitere Formatierungsoptionen für Datumswerte in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID297)
- [Tutorial zur Arbeit mit Datumswerten in Swift](https://www.raywenderlich.com/14377119-dates-and-times-in-swift)