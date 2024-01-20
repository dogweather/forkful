---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Finden der Länge eines Strings bedeutet, die Anzahl der Zeichen in diesem String zu ermitteln. Programmierer tun dies, um z.B. Berechnungen durchzuführen, Bedingungen zu prüfen oder Inhalte zu formatieren.

## Wie macht man das:

In Swift ist es ganz einfach, die Länge eines Strings zu finden. Du verwendest die `count` Eigenschaft des Strings. Hier ist ein einfaches Beispiel:

```Swift
let meinString = "Programmieren"
let laenge = meinString.count
print("Die Länge des Strings ist \(laenge).")
```

Die Ausgabe dieses Codes würde sein: Die Länge des Strings ist 13.

## Tiefer Einblick

Swift handhabt das Strings Zählen etwas anders als ältere Sprachen. Früher wurden Strings als Arrays von Zeichen gehandhabt. Man würde über diese Arrays iterieren, um ihre Länge zu finden. Swift betrachtet Strings jedoch als Unicode-skalare Werte. Daher ist es möglich, dass ein scheinbar einzelnes Zeichen (zum Beispiel ein Emoticon) als mehrere Unicode-skalare Werte betrachtet wird. Daher sollte man mit der `count` Eigenschaft von Swift das Zählen durchführen.

Es gibt auch alternative Weisen, die Länge eines Strings in Swift zu finden. Eine Möglichkeit ist es, die `utf16` Eigenschaft des Strings zu verwenden, die den String in eine Art Array aus 16-Bit-Unicode-Transformationseinheiten umwandelt. Hier ein Beispiel:

```Swift
let meinString = "Programmieren 🚀"
let laenge = meinString.utf16.count
print("Die Länge des Strings ist \(laenge).")
```

Beachte, dass bei Verwendung von `utf16` die Ausgabe für das obige Beispiel 15 ist und nicht 14, da das Emoticon als zwei separate Einheiten gezählt wird.

## Sehen Sie Auch

- Swift-String und Character Dokumentation: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- StackOverFlow-Diskussion über das Zählen von Zeichen in einem String in Swift: [https://stackoverflow.com/questions/24092884/get-number-of-characters-in-a-string-in-swift](https://stackoverflow.com/questions/24092884/get-number-of-characters-in-a-string-in-swift)