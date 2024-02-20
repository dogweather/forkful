---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:58.760847-07:00
description: "Pisanie do standardowego b\u0142\u0119du (stderr) w C# polega na kierowaniu\
  \ komunikat\xF3w o b\u0142\u0119dach i diagnostyki oddzielnie od regularnego wyj\u015B\
  cia (stdout), aby\u2026"
lastmod: 2024-02-19 22:04:54.559388
model: gpt-4-0125-preview
summary: "Pisanie do standardowego b\u0142\u0119du (stderr) w C# polega na kierowaniu\
  \ komunikat\xF3w o b\u0142\u0119dach i diagnostyki oddzielnie od regularnego wyj\u015B\
  cia (stdout), aby\u2026"
title: "Pisanie do standardowego b\u0142\u0119du"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pisanie do standardowego błędu (stderr) w C# polega na kierowaniu komunikatów o błędach i diagnostyki oddzielnie od regularnego wyjścia (stdout), aby użytkownicy i programiści mogli odróżnić normalne wyniki działania programu od powiadomień o błędach. Programiści robią to, aby debugowanie i logowanie było bardziej efektywne, co pozwala na płynniejszą operację i utrzymanie aplikacji.

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
