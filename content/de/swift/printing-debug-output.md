---
title:    "Swift: Debug-Ausgabe drucken"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Beim Entwickeln von Apps kann es oft schwierig sein, herauszufinden, warum der Code nicht so funktioniert wie er sollte. In solchen Fällen ist das Arbeitsmittel der Wahl oft das bedruckte Output. Lesen Sie weiter, um herauszufinden, warum es eine wichtige Praxis beim Swift-Programmieren ist.

## Wie

Um den Debug-Output in Swift zu aktivieren, gibt es verschiedene Methoden. Eine Möglichkeit ist die Verwendung der `print()` Funktion. Hier ist ein Beispielcode, der die Verwendung von `print()` zeigt:

```Swift
let name = "Max"
print("Hallo \(name), wie geht es dir?")
```
Dieser Code gibt "Hallo Max, wie geht es dir?" in der Konsole aus.

Eine andere Möglichkeit ist die Verwendung des `debugPrint()` Befehls, der zusätzliche Informationen wie z.B. der Datentyp oder die Speicheradresse des Werts liefert. Hier ist ein Beispielcode, der `debugPrint()` verwendet:

```Swift
let age = 25
debugPrint(age)
```
Dieser Code gibt "25" aus, aber auch zusätzliche Informationen wie z.B. "Int(25)" und "0x7fb37bc3b3f0" (Speicheradresse des Werts).

Es ist auch möglich, benutzerdefinierte Debug-Ausgaben mit dem `CustomStringConvertible` Protokoll zu erstellen. Hier ist ein Beispielcode, der dies zeigt:

```Swift
struct Person: CustomStringConvertible {
    var name: String
    var age: Int
    
    var description: String {
        return "\(name) is \(age) years old."
    }
}

let person = Person(name: "Emma", age: 30)
print(person)
```
Dieser Code gibt "Emma is 30 years old." aus.

## Tiefentauchen

Zusätzlich zu den oben genannten Methoden können Entwickler auch die `assert()` Funktion verwenden, um Bedingungen zu überprüfen und bei einem Fehler eine Debug-Ausgabe auszugeben. Eine weitere nützliche Funktion ist `precondition()`, die dafür sorgt, dass bestimmte Bedingungen erfüllt sind, bevor der Code ausgeführt wird.

Es ist auch wichtig, den Debug-Output richtig zu formatieren, um ihn leichter lesbar zu machen. Dazu kann das `separator` Argument verwendet werden, um zwischen den einzelnen Ausgaben zu unterscheiden, sowie `terminator` um anzugeben, was am Ende jeder Ausgabe stehen soll.

## Siehe auch

- [Swift Dokumentation über Debugging](https://docs.swift.org/swift-book/LanguageGuide/Debugging.html)
- [Verwendung von `print()` und `debugPrint()`](https://www.hackingwithswift.com/read/15/4/print-and-debug-print-how-to-send-output-to-the-xcode-debugger)
- [Benutzerdefinierte Debug-Ausgaben mit `CustomStringConvertible`](https://www.hackingwithswift.com/example-code/language/how-to-customize-debug-descriptions-using-customstringconvertible)
- [Verwenden von `assert()` und `precondition()`](https://www.swiftbysundell.com/posts/debugging-in-swift-using-assertions-and-preconditions)