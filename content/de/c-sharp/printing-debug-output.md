---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Drucken von Debug-Ausgaben bedeutet im Grunde, die Informationen im Code zur Problembehandlung anzuzeigen. Programmierer machen dies, um Fehler und unerwartete Verhaltensweisen einfacher zu erkennen und zu beheben.

## So geht's:
Um Debug-Ausgaben in C# zu drucken, verwenden wir `Debug.WriteLine()`. Hier sehen Sie, wie das aussieht:

```C#
using System.Diagnostics;

class Program
{
    static void Main()
    {
        int testVar = 5;
        Debug.WriteLine("Der Wert der Testvariablen beträgt: " + testVar);
    }
}
```
Der output lautet: 

```Der Wert der Testvariablen beträgt: 5```

Um mehrere Variablen auszugeben, verwenden Sie Kommas:

```C#
Debug.WriteLine("Mehrere Werte: {0}, {1}, {2}", var1, var2, var3);
```

## Vertiefung

Die Verwendung von `Debug.WriteLine()` hat eine lange Geschichte, die bis zu den Anfängen der Programmierung zurückreicht. Es ist ein einfaches, unverzichtbares Tool, das aus der Notwendigkeit heraus entstanden ist, ein tieferes Verständnis für den Code und dessen Funktion zu erhalten.

Alternativen zu `Debug.WriteLine()` sind verschiedene Logging-Bibliotheken wie log4net oder Serilog, die umfangreichere Funktionen bieten. Dennoch bleibt Debug.WriteLine wegen seiner Einfachheit und Direktheit besonders für das schnelle Debugging sehr nützlich.

Die Implementierung von `Debug.WriteLine()` ist einfach. Das `System.Diagnostics`-Namespace muss eingebunden werden, und schon können Sie Debug-Ausgaben schreiben. Beachten Sie jedoch, dass diese Ausgaben nur im Debug-Modus und nicht in ausführbaren Release-Versionen angezeigt werden.

## Siehe Auch

Für weitere Informationen über Drucken von Debug-Ausgaben in C#, finden Sie hier einige nützliche Quellen:

- [MSDN: Debug-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.diagnostics.debug?view=net-5.0)
- [Serilog: strukturiertes Logging für .NET](https://serilog.net/)
- [log4net: populäre Logging-Bibliothek](https://logging.apache.org/log4net/)