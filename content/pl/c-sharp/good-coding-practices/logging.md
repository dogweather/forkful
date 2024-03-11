---
date: 2024-01-26 01:01:13.907882-07:00
description: "Logowanie to proces rejestrowania zdarze\u0144 aplikacji i danych wyj\u015B\
  ciowych podczas jej dzia\u0142ania. Programi\u015Bci loguj\u0105, aby diagnozowa\u0107\
  \ b\u0142\u0119dy, monitorowa\u0107\u2026"
lastmod: '2024-03-11T00:14:08.597891-06:00'
model: gpt-4-1106-preview
summary: "Logowanie to proces rejestrowania zdarze\u0144 aplikacji i danych wyj\u015B\
  ciowych podczas jej dzia\u0142ania. Programi\u015Bci loguj\u0105, aby diagnozowa\u0107\
  \ b\u0142\u0119dy, monitorowa\u0107\u2026"
title: "Rejestrowanie zdarze\u0144"
---

{{< edit_this_page >}}

## Co i dlaczego?
Logowanie to proces rejestrowania zdarzeń aplikacji i danych wyjściowych podczas jej działania. Programiści logują, aby diagnozować błędy, monitorować wydajność oprogramowania, śledzić działania użytkowników oraz utrzymywać zgodność z normami bezpieczeństwa i biznesowymi.

## Jak to zrobić:
W C#, można użyć wbudowanej przestrzeni nazw `System.Diagnostics` lub bibliotek stron trzecich takich jak NLog czy log4net. Oto krótki przykład z wykorzystaniem interfejsu `ILogger`, dostępnego w .NET Core:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("To jest informacyjna wiadomość.");
        logger.LogWarning("To jest wiadomość ostrzegawcza.");
        logger.LogError("To jest wiadomość o błędzie.");
    }
}
```

Przykładowe wyjście:
```
info: Program[0]
      To jest informacyjna wiadomość.
warn: Program[0]
      To jest wiadomość ostrzegawcza.
fail: Program[0]
      To jest wiadomość o błędzie.
```

## Dogłębna analiza
Historia logowania w rozwoju oprogramowania jest niemal tak stara jak samo programowanie; ewoluowała od prostych poleceń wydruku do zaawansowanych, konfigurowalnych systemów. Początkowo logowanie odbywało się przez zapisywanie do plików lub konsoli, ale rozrosło się to do bardziej złożonych struktur, takich jak systemy agregacji logów i platformy śledzenia rozproszonego (jak stos ELK czy Jaeger).

Alternatywy dla wbudowanego logowania w .NET obejmują biblioteki stron trzecich:
- **NLog**: wszechstronny i łatwy w konfiguracji, z wieloma funkcjami do kierowania, formatowania i filtrowania logów.
- **log4net**: inspirowany biblioteką Java log4j, jest wysoce konfigurowalny z XML i obsługuje różnorodność repozytoriów logów.

Jeśli chodzi o szczegóły implementacji, wybór abstrakcji logowania (jak Microsoft.Extensions.Logging) oraz podstawowego dostawcy logowania może znacząco wpłynąć na wydajność i niezawodność Twojej aplikacji. Kluczowe jest właściwe skonfigurowanie poziomów logowania oraz upewnić się, że zapisywanie logów nie stanie się wąskim gardłem.

Ponadto, strukturalne logowanie - gdzie rejestruje się nie tylko łańcuchy znaków, ale pary klucz-wartość lub obiekty - umożliwia bardziej precyzyjne i użyteczne logi, które są łatwiejsze do zapytań i analizy.

## Zobacz także
- [Dokumentacja Microsoft.Extensions.Logging](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [Dokumentacja NLog](https://nlog-project.org/documentation/)
- [Dokumentacja log4net](https://logging.apache.org/log4net/)
- [Dokumentacja Serilog](https://serilog.net/) (przykład logowania strukturalnego)
