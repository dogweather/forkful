---
title:                "Swift: Fehlermeldungen ausgeben"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Der Einsatz von Debug-Ausgaben ist eine gängige Praxis in der Entwicklung von Informatikprojekten. Sie ermöglichen es Entwicklerinnen und Entwicklern, den Ablauf des Codes zu verfolgen und potenzielle Fehler zu identifizieren. In diesem Blogbeitrag werden wir uns genauer damit beschäftigen, wie Debug-Ausgaben in Swift-Code verwendet werden können.

## Wie geht's

Um Debug-Ausgaben in Swift zu verwenden, können wir den Befehl "print()" verwenden. Dieser Befehl gibt den angegebenen Wert oder Variablenwert auf der Konsole aus. Im folgenden Beispiel geben wir den Wert "Hello World!" aus:

```Swift
print("Hello World!")
```

Die Ausgabe auf der Konsole würde dann wie folgt aussehen:

```
Hello World!
```

Wir können auch Variablenwerte ausgeben, indem wir diese in die Klammer von "print()" einfügen. Nehmen wir an, wir haben eine Variable "name" mit dem Wert "Max":

```Swift
let name = "Max"
print(name)
```

Die Ausgabe wäre dann:

```
Max
```

## Tiefergehende Informationen

Es gibt verschiedene Techniken, um Debug-Ausgaben noch effektiver zu nutzen. Eine Möglichkeit ist es, im Code verschiedene Markierungen zu setzen, die es uns ermöglichen, gezielt bestimmte Ausgaben auszugeben. Dies kann hilfreich sein, um zum Beispiel Input-Werte oder den Ablauf von Schleifen zu verfolgen.

Ein weiteres nützliches Feature ist der Einsatz von "conditional printing". Dabei wird die Ausgabe nur ausgeführt, wenn eine bestimmte Bedingung erfüllt ist. Dies kann dazu beitragen, dass die Konsolenausgabe übersichtlicher wird und unnötige Ausgaben vermieden werden.

## Siehe auch

- [Apple's Debugging Guide](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/debugging_tools.html#//apple_ref/doc/uid/TP40015022-CH5-SW2)
- [Ray Wenderlich's Debugging Tutorial](https://www.raywenderlich.com/773-debugging-tips-and-tricks-swift-programming-25)
- [Useful Debugging Techniques in Swift](https://medium.com/@jberczel/useful-debugging-techniques-in-swift-d23b32e8ba8b)