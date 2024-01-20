---
title:                "String in Großbuchstaben umwandeln"
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
String-Kapitalisierung verwandelt alle Buchstaben in Großbuchstaben oder macht den ersten Buchstaben eines Satzes groß. Das hilft bei der Konsistenz in Nutzeroberflächen oder beim Vergleichen von Text ohne Groß- und Kleinschreibung zu beachten.

## How to: (Wie geht das?)
```Swift
let smallTalk = "hallo, wie geht's?"
let shouting = smallTalk.uppercased() // "HALLO, WIE GEHT'S?"
let properGreeting = smallTalk.capitalized // "Hallo, Wie Geht's?"

// Beispiel-Ausgabe
print(shouting)      // HALLO, WIE GEHT'S?
print(properGreeting) // Hallo, Wie Geht's?
```

## Deep Dive (Tiefgang)
String-Kapitalisierung in Swift verwendet Funktionen wie `uppercased()` und `capitalized`. `uppercased()` verwandelt jeden Buchstaben in Großbuchstaben, während `capitalized` nur den ersten Buchstaben jedes Wortes groß macht. Seit Swift 1.0 ist das möglich, und es hilft bei der Textverarbeitung enorm.

Alternativen gibt es nicht wirklich, da diese Funktionen Teil der Swift-Standardbibliothek sind und alles Nötige abdecken. Intern verwenden diese Methoden Unicode, um auch mit nicht-lateinischen Alphabeten umgehen zu können.

Die Implementierung hängt von `Locale` ab – zum Beispiel werden bei `capitalized` mit einer türkischen Locale 'i's zu 'İ' anstatt 'I'. Das ist wichtig für korrekte Darstellung von Texten in verschiedenen Sprachen.

## See Also (Siehe auch)
- Swift Standard Library Reference: [Apple's Documentation](https://developer.apple.com/documentation/swift/string)
- Unicode Standard: [unicode.org](https://www.unicode.org/)
- Locale in Swift: [NSLocale Documentation](https://developer.apple.com/documentation/foundation/nslocale)