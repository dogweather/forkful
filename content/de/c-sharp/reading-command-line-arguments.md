---
title:                "Lesen von Befehlszeilen-Argumenten"
html_title:           "C#: Lesen von Befehlszeilen-Argumenten"
simple_title:         "Lesen von Befehlszeilen-Argumenten"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was ist das und warum?

Das Lesen von Befehlszeilenargumenten ist ein häufiger Bestandteil der Programmierung in C#. Es ermöglicht einem Programm, Eingabedaten direkt aus der Befehlszeile zu erhalten. Das ist hilfreich, wenn man zum Beispiel ein Programm ausführen möchte, das verschiedene Parameter erfordert.

## Wie geht das?

Es gibt verschiedene Möglichkeiten, Befehlszeilenargumente in C# auszulesen. Eine Möglichkeit ist die Verwendung der Klasse `Environment`, die eine Methode namens `GetCommandLineArgs` bereitstellt. Hier ist ein Beispiel:

```C#
string[] arguments = Environment.GetCommandLineArgs();

foreach(string argument in arguments)
{
  Console.WriteLine(argument);
}
```

Die Ausgabe wird alle Befehlszeilenargumente anzeigen, die beim Start des Programms übergeben wurden. Zum Beispiel, wenn das Programm mit den Argumenten `argument1 argument2 argument3` aufgerufen wird, wird die Ausgabe entsprechend sein:

`argument1 argument2 argument3`

## Tiefer in die Materie eintauchen

Das Lesen von Befehlszeilenargumenten existiert schon seit den frühen Tagen der Programmierung. Es ist eine praktische Möglichkeit, Benutzern die Möglichkeit zu geben, direkt mit einem Programm zu interagieren, ohne eine grafische Benutzeroberfläche verwenden zu müssen.

Es gibt auch andere Möglichkeiten, Befehlszeilenargumente in C# zu lesen, wie z.B. die Verwendung von Attributen oder das Parsen von benutzerdefinierten Argumenten.

Die Implementierung der `GetCommandLineArgs`-Methode basiert auf der `Win32 GetCommandLine`-Funktion, die die Befehlszeile einer Anwendung ausgibt.

## Weitere Informationen

Für weitere Informationen über das Lesen von Befehlszeilenargumenten in C# können folgende Links hilfreich sein:

- [Die offizielle Dokumentation zur Environment-Klasse in C#](https://docs.microsoft.com/de-de/dotnet/api/system.environment)
- [Ein Tutorial zum Lesen von Befehlszeilenargumenten in C# von The Code Project](https://www.codeproject.com/Articles/3111/C-Command-Line-Argument-Parsing-Redux)