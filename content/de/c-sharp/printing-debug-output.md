---
title:                "C#: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind ein wichtiges Instrument für Entwickler, um Fehler in ihrem Code zu finden und zu beheben. Durch das Drucken von debug-Ausgaben können Entwickler den Zustand ihres Codes zu einem bestimmten Zeitpunkt nachvollziehen und so potenzielle Fehlerquellen identifizieren.

## Wie

Das Drucken von debug-Ausgaben in C# ist einfach und kann auf verschiedene Arten erfolgen. Hier sind zwei Beispiele:

```C#
// Beispiel 1: Verwendung von Console.WriteLine()
int num = 10;
Console.WriteLine($"num hat den Wert von {num}");

// Output: num hat den Wert von 10
```

```C#
// Beispiel 2: Verwendung von Debug.WriteLine()
Debug.WriteLine("Dies ist eine Debug-Ausgabe");

// Output in der Debug-Konsole: Dies ist eine Debug-Ausgabe
```

Diese Beispiele zeigen, dass wir entweder die `Console.WriteLine()`-Methode oder die `Debug.WriteLine()`-Methode verwenden können, um debug-Ausgaben zu drucken.

## Deep Dive

In C# gibt es verschiedene Arten, wie wir debug-Ausgaben formatieren können. Wir können z.B. die Platzhalter-Syntax (`${}`) oder die String-Interpolation (`$""`) verwenden, um Variablenwerte in unsere Ausgabe einzubinden.

```C#
// Platzhalter-Syntax
string name = "Max Mustermann";
Console.WriteLine("Mein Name ist {0}", name);

// Output: Mein Name ist Max Mustermann

// String-Interpolation
int num1 = 5;
int num2 = 10;
Console.WriteLine($"Das Ergebnis von {num1} + {num2} ist {num1 + num2}");

// Output: Das Ergebnis von 5 + 10 ist 15
```

Zusätzlich können wir durch das Setzen von Breakpoints in Visual Studio und das Hinzufügen von Debug-Messages unsere Ausgabe noch genauer kontrollieren und beobachten.

## Siehe auch

- [Debug-Ausgaben in C#](https://docs.microsoft.com/de-de/visualstudio/debugger/using-the-debugger-window?view=vs-2019)
- [Tipps und Tricks für das Debuggen in C#](https://stackify.com/csharp-debugging-tips/)
- [Fehlerbehebung in C#](https://www.codeproject.com/Articles/6948/Debugging-in-C-A-tutorial)