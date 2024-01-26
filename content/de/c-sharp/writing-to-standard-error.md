---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf Standardfehler (stderr) ist ein Stream, um Fehlermeldungen und Diagnosen separat von regulären Ausgaben (stdout) zu behandeln. Programmierer nutzen es, um die Robustheit und Wartbarkeit von Programmen zu verbessern, indem sie betriebliche von Fehlernachrichten trennen.

## How to:
Benutze `Console.Error` um auf `stderr` zu schreiben. So bleibt `stdout` nur für erwartete Ausgaben.

```C#
using System;

class Program {
    static void Main() {
        Console.WriteLine("Standardausgabe: Programm startet.");
        Console.Error.WriteLine("Fehler: Etwas ist schiefgelaufen.");
    }
}
```
Ausgabe:
```
Standardausgabe: Programm startet.
Fehler: Etwas ist schiefgelaufen. 
```
Bemerke, dass die Reihenfolge in realen Anwendungen je nach Puffer anders sein kann.

## Deep Dive:
Historisch kommen `stdout` und `stderr` aus der Unix-Welt. Beide können umgeleitet werden, um etwa Fehler in eine Datei zu schreiben, während normale Ausgaben auf dem Bildschirm bleiben. In C# sind `Console.Out` und `Console.Error` zwei unabhängige `TextWriter`-Objekte. Beim Schreiben von Konsolenanwendungen oder beim Logging ist die Unterscheidung hilfreich, besonders wenn du mit Pipes und Redirection in Shells arbeitest.

## See Also:
- [MSDN Dokumentation zu Console.Error](https://docs.microsoft.com/dotnet/api/system.console.error)
- [UNIX Standard Streams (Englisch)](https://en.wikipedia.org/wiki/Standard_streams)
- [Weiterführende Informationen zu TextWriter (Englisch)](https://docs.microsoft.com/dotnet/api/system.io.textwriter)
