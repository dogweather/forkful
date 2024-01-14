---
title:    "Swift: Verwendung von regulären Ausdrücken"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Warum

In der heutigen Zeit ist es wichtig, als Entwickler*in ein breites Wissen über verschiedene Programmiersprachen und Technologien zu haben. Eine dieser nützlichen Techniken sind reguläre Ausdrücke, auch bekannt als "regular expressions", welche eine leistungsstarke Möglichkeit bieten, Textmuster in Strings zu suchen und zu manipulieren.

## Wie geht das?

Das Verständnis und die Anwendung von regulären Ausdrücken in Swift kann zunächst einschüchternd wirken, aber keine Sorge - es ist einfacher als es aussieht! Beginnen wir mit der Erstellung eines neuen Projekts in Xcode und importieren das Foundation-Framework, welches die erforderlichen Klassen und Methoden für die Verwendung von regulären Ausdrücken bereitstellt.

Als nächstes erstellen wir einen String, in dem wir nach einem bestimmten Muster suchen wollen:

```Swift
let string = "Meine Telefonnummer ist 123-456-7890."
```

Um nun nach dem Muster "123-456-7890" zu suchen, können wir die Methode `range(of:options:range:locale:)` von `String` verwenden und als Option `.regularExpression` angeben:

```Swift
let range = string.range(of: "[0-9]{3}-[0-9]{3}-[0-9]{4}", options: .regularExpression)
```

Dies wird uns den Bereich des Strings zurückgeben, der dem angegebenen Muster entspricht. Um nun tatsächlich den Teil des Strings zu bekommen, der dem Muster entspricht, können wir die Methode `substring(with:)` verwenden:

```Swift
let matchedString = string.substring(with: range!)
print(matchedString) // Ausgabe: 123-456-7890
```

Es gibt noch viele weitere Methoden und Möglichkeiten, reguläre Ausdrücke in Swift zu verwenden. Mach dir keine Sorgen, wenn es anfangs etwas kompliziert erscheint - mit Übung wirst du die nützlichen Fähigkeiten von regulären Ausdrücken sicherlich zu schätzen wissen!

## Tiefergehende Informationen

Neben der Verwendung von regulären Ausdrücken zur Suche und Manipulation von Text, bieten sie auch Möglichkeiten zur Validierung von Formulardaten oder der Überprüfung von Passwörtern. Es gibt auch eine Vielzahl von speziellen Zeichen und Operatoren, die verwendet werden können, um komplexere Muster zu definieren.

Eine ausführliche Anleitung zur Verwendung von regulären Ausdrücken findest du in der offiziellen Dokumentation von Apple unter [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression) und in der Umsetzung von regulären Ausdrücken in Swift unter [NSRegularExpression.Compatibility](https://developer.apple.com/documentation/swift/nsregularexpression/compatibility).

## Siehe auch

- [NSRegularExpression Dokumentation von Apple](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [NSRegularExpression.Compatibility Dokumentation von Apple](https://developer.apple.com/documentation/swift/nsregularexpression/compatibility)
- [RegEx101 - Ein nützliches Tool zum Testen von regulären Ausdrücken](https://regex101.com/)