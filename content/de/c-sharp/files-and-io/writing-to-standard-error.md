---
title:                "Schreiben auf Standardfehler"
aliases: - /de/c-sharp/writing-to-standard-error.md
date:                  2024-02-03T19:32:39.174973-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schreiben auf Standardfehler"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf die Standardfehlerausgabe (stderr) in C# beinhaltet die Weiterleitung von Fehlermeldungen und Diagnosen, getrennt von der regulären Ausgabe (stdout), um Benutzern und Entwicklern zu helfen, zwischen normaler Programmausgabe und Fehlerbenachrichtigungen zu unterscheiden. Programmierer tun dies, um das Debugging und Logging effizienter zu machen, was einen reibungsloseren Betrieb und Wartung von Anwendungen ermöglicht.

## Wie:
In C# kann das Schreiben auf die Standardfehlerausgabe mittels des `Console.Error`-Streams erreicht werden. Dieser Stream wird speziell für Fehlermeldungen und Diagnosen verwendet. Hier ist ein einfaches Beispiel:

```csharp
Console.Error.WriteLine("Error: Failed to process the request.");
```

Beispielausgabe (an stderr):
```
Error: Failed to process the request.
```

In Szenarien, in denen Sie möglicherweise eine Drittanbieterbibliothek verwenden, die erweiterte Logging-Fähigkeiten bietet, wie `Serilog` oder `NLog`, können Sie diese Bibliotheken konfigurieren, um Fehlerprotokolle an stderr zu schreiben. Während diese Beispiele sich auf einfache Konsolenumleitungen konzentrieren, denken Sie daran, dass in Produktionsanwendungen Logging-Frameworks viel robustere Fehlerbehandlungs- und Ausgabeoptionen bieten. Hier ist ein einfaches Beispiel mit `Serilog`:

Zuerst installieren Sie das Serilog-Paket und dessen Console-Sink:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Konfigurieren Sie dann Serilog so, dass es auf stderr schreibt:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("Das ist eine normale Nachricht.");
Log.Error("Das ist eine Fehlermeldung.");
```

Beispielausgabe (an stderr für die Fehlermeldung):
```
[15:04:20 ERR] Das ist eine Fehlermeldung.
```

Hinweis: Die Konfiguration `standardErrorFromLevel` in Serilogs Konsolen-Sink leitet alle Log-Ereignisse auf der angegebenen Ebene (im diesem Fall Fehler) oder höher zur Standardfehlerausgabe um, während Nachrichten niedrigerer Ebene, wie Informationen, auf die Standardausgabe geschrieben werden.
