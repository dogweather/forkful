---
date: 2024-01-20 17:28:32.635604-07:00
description: 'Como Fazer: .'
lastmod: '2024-03-13T22:44:46.597236-06:00'
model: gpt-4-1106-preview
summary: .
title: Calculando uma data no futuro ou passado
weight: 26
---

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
