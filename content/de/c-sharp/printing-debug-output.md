---
title:                "Ausgabe von Debugging-Ergebnissen"
html_title:           "C#: Ausgabe von Debugging-Ergebnissen"
simple_title:         "Ausgabe von Debugging-Ergebnissen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Es gibt verschiedene Gründe, warum man sich für die Ausgabe von Debug-Informationen im Code interessieren könnte. Einer der Hauptgründe ist, dass es hilft, Probleme im Programm besser zu verstehen und zu debuggen. Auch kann es nützlich sein, um den Ablauf des Codes zu verfolgen und die Korrektheit der Ausführung zu überprüfen.

## Wie es geht

Um Debug-Ausgaben in C# einzubauen, gibt es verschiedene Methoden. Eine simple Möglichkeit ist die Verwendung der `Console.WriteLine()` Methode, um eine Nachricht auf der Konsole auszugeben. Hier ein Beispiel:

```C#
int num = 5;
Console.WriteLine("Die Variable num hat den Wert: " + num);
```

Dies würde folgende Ausgabe erzeugen:

```
Die Variable num hat den Wert: 5
```

Eine weitere Möglichkeit ist die Verwendung des `Debug` Klassen aus dem `System.Diagnostics` Namespace. Diese bietet verschiedene Methoden, um Debug-Ausgaben zu formatieren und zu verwalten. Hier ein Beispiel:

```C#
int num1 = 5;
int num2 = 8;
Debug.WriteLine($"Die Summe von {num1} und {num2} ist {num1 + num2}");
```

Dies würde folgende Ausgabe erzeugen:

```
Die Summe von 5 und 8 ist 13
```

## Tiefer Einblick

Es gibt noch viele weitere Möglichkeiten, Debug-Ausgaben in C# zu nutzen. Dazu zählen unter anderem die Verwendung von Logger-Bibliotheken wie NLog oder Log4net, die es ermöglichen, die Ausgaben zu filtern und in verschiedene Protokolldateien oder -kanäle zu schreiben. Auch ist es möglich, eigene Debug-Ausgabefunktionen zu entwickeln, die spezifische Anforderungen erfüllen.

Generell empfiehlt es sich, Debug-Ausgaben nur in Entwicklungsumgebungen einzubauen und diese für Produktivumgebungen zu deaktivieren, um die Performance des Programms nicht zu beeinträchtigen.

## Siehe auch

- [Microsoft Dokumentation zu Debugging in C#](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour?view=vs-2019)
- [Tutorial: Einführung in Debugging in C#](https://www.tutorialspoint.com/csharp/csharp_debugging.htm)
- [NLog - eine beliebte Logger-Bibliothek für C#](https://nlog-project.org/)