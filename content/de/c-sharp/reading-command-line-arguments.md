---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# C# Befehlszeilenargumente lesen: Ein Leitfaden

## Was & Warum?

Die Befehlszeilenargumente sind Parameter, die einem Programm beim Start übergeben werden. Sie sind nützlich, um das Verhalten des Programms zu personalisieren oder ihm Daten zu übermitteln, ohne eine Benutzereingabe zu erfordern.

## So macht man das:

Wenn du Befehlszeilenargumente in einem C#-Programm lesen willst, musst du auf die `args`-Parameter des `Main`-Verfahrens zugreifen.

```C#
static void Main(string[] args)
{
    Console.WriteLine($"Es gibt {args.Length} Argument(e).");
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine($"Argument {i + 1}: {args[i]}");
    }
}
```

Wenn du dieses Programm mit den Argumenten `Erstes Zweites Drittes` startest, würde es so aussehen:

```
Es gibt 3 Argument(e).
Argument 1: Erstes
Argument 2: Zweites
Argument 3: Drittes
```

## Deep Dive

Befehlszeilenargumente sind ein Konzept, das von den Anfängen der Programmierung stammt, als jedes Programm über eine Kommandozeile gestartet wurde. Heutzutage gibt es in C# auch alternative Möglichkeiten, wie die Verwendung von Konfigurationsdateien oder Umgebungsvariablen, die jedoch von den Projekterfordernissen abhängen.

Die `args`-Parameter im `Main`-Verfahren sind eine Mischung von `string`-Array, die durch Leerzeichen getrennte Elemente des Befehls enthält, dem das Programm gestartet wurde.

## Siehe auch

- ["Main" und die Befehlszeilenargumente (C#-Programmierhandbuch)](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/main-and-command-args/)
- [Umgebungsvariablen in C# nutzen](https://docs.microsoft.com/de-de/dotnet/api/system.environment.getenvironmentvariable)