---
title:                "C#: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum
Die Verwendung von Befehlszeilenargumenten kann eine effektive Möglichkeit sein, um die Benutzerfreundlichkeit und Funktionalität Ihrer C#-Anwendungen zu verbessern. Es ermöglicht Benutzern, bestimmte Konfigurationen oder Optionen beim Starten Ihrer Anwendung anzugeben, was zu einer personalisierten und maßgeschneiderten Erfahrung führt.

## Wie
Wenn Sie Ihre C#-Anwendung über die Befehlszeile ausführen, können Sie den `Main()`-Methode die `string[] args`-Parameter übergeben, der alle Befehlszeilenargumente als Array enthält. Hier ist ein Beispiel, wie Sie diese Argumente auslesen können:

```C#
static void Main(string[] args)
{
    // Ausgabe der Anzahl der übergebenen Argumente
    Console.WriteLine("Es wurden {0} Argumente übergeben.", args.Length);

    // Ausgabe der einzelnen Argumente
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine("Argument {0}: {1}", i, args[i]);
    }
}
```

Wenn Sie beispielsweise diese Anwendung mit den Befehlszeilenargumenten `Hallo` und `Welt` ausführen, erhalten Sie folgende Ausgabe:

```
Es wurden 2 Argumente übergeben.
Argument 0: Hallo
Argument 1: Welt
```

Sie können auch optionale Argumente mit verschiedenen Datentypen definieren, indem Sie die `Parse()`-Methode verwenden. Hier ist ein Beispiel für einen optionalen `int`-Parameter:

```C#
static void Main(string[] args)
{
    // Standardwert für den optionalen Parameter
    int optional = 10;

    // Überprüfung, ob ein optionales Argument übergeben wurde
    if (args.Length > 0)
    {
        // Verwenden der Parse()-Methode, um den String in einen int zu konvertieren
        optional = int.Parse(args[0]);
    }

    Console.WriteLine("Optional: {0}", optional);
}
```

Wenn Sie die Anwendung mit dem Befehlszeilenargument `5` ausführen, erhalten Sie folgende Ausgabe:

```
Optional: 5
```

## Deep Dive
Es gibt noch viele weitere Möglichkeiten, um Befehlszeilenargumente in C# zu nutzen. Hier sind einige andere Themen, die Sie bei Ihrer Vertiefung in dieses Thema erkunden können:

- Die Verwendung der `Environment.GetCommandLineArgs()`-Methode, um alle Argumente, einschließlich der Anwendungspfad, zu erhalten.

- Die Unterstützung von Flag-Argumenten, die als Schalter zwischen Aktivieren und Deaktivieren von Optionen dienen können.

- Die Implementierung von Optionen für Befehlszeilenargumente, um komplexe Konfigurationen zu ermöglichen.

## Siehe auch
- [Command Line Arguments in C#](https://www.tutorialspoint.com/command-line-arguments-in-c-sharp)
- [Reading Command-Line Arguments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Using Command-Line Arguments in C#](https://www.c-sharpcorner.com/UploadFile/1e050f/using-command-line-arguments-in-C-Sharp/)