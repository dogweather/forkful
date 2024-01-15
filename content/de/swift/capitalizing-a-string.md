---
title:                "Eine Zeichenkette großschreiben"
html_title:           "Swift: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Hast du dich jemals gefragt, warum es wichtig ist, Strings in Swift zu versalen? String-Operationen sind ein wichtiger Bestandteil der Programmierung und das Versalen von Strings ist eine einfache Möglichkeit, sie lesbarer zu machen. Es verleiht deinem Code auch eine professionellere und konsistente Ästhetik.

## Wie geht's

Um einen String in Swift zu versalen, kannst du die `uppercased()` Methode verwenden:

```Swift
let greeting = "hallo welt"
let capitalizedGreeting = greeting.uppercased()

print(capitalizedGreeting) // Ausgabe: HALLO WELT
```

Du kannst auch die `capitalized()` Methode verwenden, um nur den ersten Buchstaben eines Strings zu versalen:

```Swift
let word = "programmierung"
let capitalizedWord = word.capitalized()

print(capitalizedWord) // Ausgabe: Programmierung
```

## Tiefergehende Informationen

Es gibt noch weitere Möglichkeiten, Strings in Swift zu versalen. Zum Beispiel kannst du die `capitalized(with:)` Methode verwenden, um spezifische Lokalisierungen für die Großschreibung zu wählen. Du kannst auch den `uppercased()` und `capitalized()` Methoden Attribute wie `.locale` oder `.whitespace` hinzufügen, um deine gewünschte Formatierung zu erreichen.

Es ist auch wichtig zu beachten, dass es in Swift keine `lowercased()` Methode gibt, da Strings standardmäßig in Kleinbuchstaben sind. Stattdessen kannst du die `lowercased()` Methode verwenden, um einen String in Kleinbuchstaben zu verwandeln.

## Siehe auch

- [Apple documentation on capitalizing strings](https://developer.apple.com/documentation/foundation/nsstring/1415464-capitalized)
- [Hacking with Swift tutorial on capitalizing strings](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)
- [Ray Wenderlich tutorial on manipulating strings in Swift](https://www.raywenderlich.com/7585-beginning-swift-strings)