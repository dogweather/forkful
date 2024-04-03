---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:58.760847-07:00
description: "Jak to zrobi\u0107: W C#, pisanie do standardowego b\u0142\u0119du mo\u017C\
  na osi\u0105gn\u0105\u0107 za pomoc\u0105 strumienia `Console.Error`. Strumie\u0144\
  \ ten s\u0142u\u017Cy specjalnie do komunikat\xF3w o\u2026"
lastmod: '2024-03-13T22:44:35.424765-06:00'
model: gpt-4-0125-preview
summary: "W C#, pisanie do standardowego b\u0142\u0119du mo\u017Cna osi\u0105gn\u0105\
  \u0107 za pomoc\u0105 strumienia `Console.Error`."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:
W C#, pisanie do standardowego błędu można osiągnąć za pomocą strumienia `Console.Error`. Strumień ten służy specjalnie do komunikatów o błędach i diagnoz. Oto podstawowy przykład:

```csharp
Console.Error.WriteLine("Error: Failed to process the request.");
```

Przykładowe wyjście (do stderr):
```
Error: Failed to process the request.
```

W scenariuszach, w których możesz używać biblioteki innej firmy, która oferuje zaawansowane możliwości logowania, jak `Serilog` czy `NLog`, możesz skonfigurować te biblioteki, aby zapisywały logi błędów do stderr. Chociaż te przykłady skupiają się na prostej przekierowaniu konsoli, pamiętaj, że w aplikacjach produkcyjnych, frameworki do logowania oferują dużo solidniejsze opcje obsługi błędów i wyjścia. Oto prosty przykład z `Serilogiem`:

Najpierw zainstaluj pakiet Serilog i jego zlew do konsoli:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Następnie skonfiguruj Serilog do pisania do stderr:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("To jest normalna wiadomość.");
Log.Error("To jest wiadomość o błędzie.");
```

Przykładowe wyjście (do stderr dla wiadomości o błędzie):
```
[15:04:20 ERR] To jest wiadomość o błędzie.
```

Uwaga: Konfiguracja `standardErrorFromLevel` w zlewie konsolowym Serilog przekierowuje wszystkie zdarzenia logowania na określonym poziomie (Błąd, w tym przypadku) lub wyższym do strumienia błędu standardowego, podczas gdy komunikaty na niższym poziomie, takie jak Informacje, są zapisywane do strumienia wyjścia standardowego.
