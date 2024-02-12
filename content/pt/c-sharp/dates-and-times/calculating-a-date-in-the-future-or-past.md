---
title:                "Calculando uma data no futuro ou passado"
aliases:
- /pt/c-sharp/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:28:32.635604-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Calcular datas no futuro ou passado é simplesmente determinar um dia específico antes ou depois de um determinado ponto no tempo. Programadores fazem isso para manipular prazos, agendas, ou até controlar períodos de validade em aplicações.

## Como Fazer:
```C#
using System;

class Program {
    static void Main() {
        DateTime hoje = DateTime.Now;
        DateTime futuro = hoje.AddDays(10); // Adiciona 10 dias à data atual
        DateTime passado = hoje.AddDays(-5); // Subtrai 5 dias da data atual

        Console.WriteLine("Hoje: " + hoje.ToString("dd/MM/yyyy"));
        Console.WriteLine("Futuro: " + futuro.ToString("dd/MM/yyyy"));
        Console.WriteLine("Passado: " + passado.ToString("dd/MM/yyyy"));
    }
}
```

Saída de exemplo:
```
Hoje: 09/04/2023
Futuro: 19/04/2023
Passado: 04/04/2023
```

## Mergulho Profundo
Trabalhar com datas é essencial na programação e C# faz isso bem com a classe `DateTime`. No passado, sistemas diferentes tinham suas próprias formas de calcular o tempo - lembre-se do bug do ano 2000? O C# facilitou esse processo com métodos intuitivos como `AddDays()`, que consideram anos bissextos e outras peculiaridades do calendário.

Há outras maneiras de calcular datas no futuro ou no passado. Podemos usar `AddMonths()`, `AddYears()`, e até `AddHours()`, `AddMinutes()` para ajustes menores. Para precisão de benchmarks ou operações em tempo real, `Stopwatch` é uma classe robusta.

Na implementação, vale lembrar que a adição e subtração de datas podem resultar em `ArgumentOutOfRangeException` se o resultado final estiver fora do alcance do tipo `DateTime`. Além disso, operações com fusos horários requerem a classe `DateTimeOffset`.

## Veja Também
- Documentação oficial da Microsoft sobre a classe `DateTime`: [Microsoft Docs: DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netframework-4.8)
