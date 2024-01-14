---
title:    "Swift: Ausgabe von Debug-Meldungen drucken"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Warum

Es gibt verschiedene Gründe, warum man beim Programmieren den sogenannten "Debug Output" verwenden würde. Einer der Hauptgründe ist, dass es hilft, Fehler oder Probleme im Code zu identifizieren und zu beheben. Durch das Ausgeben von Informationen an bestimmten Stellen im Code, können wir besser verstehen, wie unser Programm funktioniert und wo mögliche Probleme auftreten können. Debug Output ist daher ein unverzichtbares Werkzeug für jeden Entwickler.

# Wie man Debug Output verwendet

Um Debug Output zu verwenden, gibt es eine einfache Methode in Swift: die `print()` Funktion. Diese Funktion gibt einfach den übergebenen Wert oder die übergebene Variante auf der Konsole aus. Schauen wir uns ein Beispiel an:

```Swift
let name = "Max Mustermann"

print("Hello, my name is \(name)") // Ausgabe: Hello, my name is Max Mustermann
```

Hier sehen wir, wie wir mit der `print()` Funktion einen String ausgeben können, der zusätzlich eine Variable enthält. Wir können auch mehrere Werte oder Variablen in der `print()` Funktion verwenden:

```Swift
let number1 = 5
let number2 = 10

print(number1, number2) // Ausgabe: 5 10
```

Wir können auch das Format der Ausgabe anpassen, zum Beispiel durch die Verwendung von Zeilenumbrüchen oder Kommata:

```Swift
let height = 170
let weight = 65

print("Meine Größe beträgt", height, "cm und mein Gewicht beträgt", weight, "kg.") 
// Ausgabe: Meine Größe beträgt 170 cm und mein Gewicht beträgt 65 kg.
```

Debug Output ist auch nützlich, um den Wert von Variablen an bestimmten Stellen im Code zu überprüfen. So können wir sicherstellen, dass unsere Variablen den erwarteten Wert haben und unsere Anwendung wie geplant funktioniert.

# Tiefere Einblicke in Debug Output

Während das Verwenden der `print()` Funktion in Swift einfach ist, gibt es noch weitere Möglichkeiten, Debug Output effektiv zu nutzen. Zum Beispiel können wir durch die Verwendung von Bedingungen oder Schleifen unseren Debug Output auch abhängig von bestimmten Situationen ausgeben:

```Swift
let temperature = 24

if temperature > 20 {
    print("It's a warm day today.") // Ausgabe: It's a warm day today.
}
```

Oder wir können Debug Output in einem bestimmten Bereich unseres Codes unterdrücken, indem wir die `#if DEBUG` Direktive verwenden:

```Swift
#if DEBUG
print("This message will only be printed during debug builds.")
#endif
```

Es gibt auch spezielle Debugging-Tools in Xcode, die es uns ermöglichen, den Wert von Variablen oder die Ausführung unseres Codes Schritt für Schritt zu überprüfen. Diese Tools können uns helfen, komplexere und schwer zu findende Fehler zu finden.

# Siehe auch

- [Apple Dokumentation: Debugging in Xcode](https://developer.apple.com/documentation/xcode/debugging)
- [Swift Debugging Tutorial](https://www.raywenderlich.com/4331018-debugging-in-swift-a-tutorial)
- [Swift Debugging Best Practices](https://theswiftdev.com/debugging-swift-code-like-a-pro/)