---
title:    "Swift: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine wichtige Fähigkeit, die in der Swift Programmierung benötigt wird. Egal ob man unerwünschte Zeichen aus einem String entfernen möchte oder eine Validierung durchführt, das Löschen von Zeichen macht die Arbeit einfacher und effizienter.

# Wie

Um Zeichen basierend auf einem Muster zu löschen, kann man die Methode `removeAll(where:)` verwenden. Diese Methode nimmt eine Closure als Parameter an, die entscheidet, ob ein Zeichen gelöscht werden soll oder nicht. Innerhalb der Closure kann man eine Bedingung definieren, die überprüft, ob ein Zeichen dem gewünschten Muster entspricht.

```Swift
let text = "This is a boring string 😴"
// Entfernt alle Vokale aus dem String
text.removeAll { (char) -> Bool in
    return "aeiou".contains(char)
}
// Output: "Ths s  brng strng "
```

# Deep Dive

Die `removeAll(where:)` ist eine nützliche Methode, die auf der `RangeReplaceableCollection` Protokoll basiert. Sie ermöglicht es, jedes Element einer Kollektion (z.B. String, Array) basierend auf einer bestimmten Bedingung zu löschen. Dank Swift's Closures ist es möglich, maßgeschneiderte Bedingungen zu definieren und so flexibel auf verschiedene Anwendungsfälle zu reagieren.

See Also

- https://developer.apple.com/documentation/swift/rangereplaceablecollection/2946552-removeall
- https://www.swiftbysundell.com/tips/using-closures-as-parameters-in-swift/