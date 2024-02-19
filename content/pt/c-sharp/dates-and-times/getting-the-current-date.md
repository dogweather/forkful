---
aliases:
- /pt/c-sharp/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:17.155493-07:00
description: "Obter a data atual em C# envolve buscar os detalhes da data e hora atuais\
  \ do sistema. Programadores frequentemente precisam acessar essa informa\xE7\xE3\
  o para\u2026"
lastmod: 2024-02-18 23:08:58.167668
model: gpt-4-0125-preview
summary: "Obter a data atual em C# envolve buscar os detalhes da data e hora atuais\
  \ do sistema. Programadores frequentemente precisam acessar essa informa\xE7\xE3\
  o para\u2026"
title: Obtendo a data atual
---

{{< edit_this_page >}}

## O Que & Por Quê?
Obter a data atual em C# envolve buscar os detalhes da data e hora atuais do sistema. Programadores frequentemente precisam acessar essa informação para registrar logs, marcar operações com timestamp (data e hora) ou agendar tarefas dentro de aplicações, garantindo que as ações sejam cronometradas com precisão e os dados sejam marcados com timestamps exatos.

## Como fazer:
C# oferece uma maneira direta de obter a data atual usando a classe `DateTime`, que faz parte do namespace System da .NET Framework. O exemplo abaixo demonstra como obter a data atual e, opcionalmente, a hora.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Obtém apenas a data atual
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Saída: MM/dd/yyyy
        
        // Obtém a data e hora atuais
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Saída: MM/dd/yyyy HH:mm:ss

        // Obtém a data e hora atuais em UTC
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Saída: MM/dd/yyyy HH:mm:ss
    }
}
```

Em termos de bibliotecas de terceiros, NodaTime oferece uma alternativa robusta para manipulação de data e hora, incluindo a busca da data atual em diferentes calendários e zonas horárias.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Usando NodaTime para obter a data atual no calendário ISO
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Saída: yyyy-MM-dd

        // Para datas específicas de timezone
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Saída: yyyy-MM-dd
    }
}
```

Isso demonstra o uso básico com a classe `DateTime` incorporada e as capacidades aprimoradas fornecidas pelo NodaTime, especialmente úteis para aplicações que requerem o tratamento de diferentes zonas horárias ou sistemas de calendário.
