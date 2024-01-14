---
title:    "Swift: Das aktuelle Datum erhalten"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Warum

In diesem Blogbeitrag werden wir uns damit beschäftigen, wie wir das aktuelle Datum in Swift programmgesteuert erhalten können. Das Abrufen des aktuellen Datums kann nützlich sein, um datumsbezogene Funktionen in unseren Apps zu implementieren oder um Benutzern ein besseres Verständnis von Zeitabläufen zu ermöglichen.

# Wie geht's

Um das aktuelle Datum in Swift zu erhalten, können wir die `Date()`-Klasse verwenden. Schauen wir uns ein Beispiel an:

```
let currentDate = Date()
print(currentDate)
```

Die Ausgabe dieses Codes wird das aktuelle Datum und die Uhrzeit in der Konsole anzeigen. Durch das Erstellen einer Instanz der `Date()`-Klasse werden wir den aktuellen Zeitstempel erhalten.

# Tiefere Einblicke

Die `Date()`-Klasse ist Teil der `Foundation`-Framework und bietet viele Methoden und Eigenschaften, die uns helfen können, das Datum und die Uhrzeit in unseren Apps zu manipulieren. Zum Beispiel können wir durch die Verwendung von `Calendar`-Klasse und deren Methoden das Datum in verschiedene Zeitformate konvertieren, Zeitzonen anwenden oder bestimmte Datumskomponenten extrahieren.

# Siehe auch

- [Apple Dokumentation zu Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial: Datum in Swift](https://www.raywenderlich.com/6617737-dates-and-times-in-swift-getting-started)
- [Swift-Playground: Datum und Uhrzeit](https://developer.apple.com/swift/blog/?id=37)