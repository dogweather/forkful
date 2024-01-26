---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:13:28.458325-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Obter a data atual em C# é um jeito de saber que dia é hoje dentro do seu programa - simples assim. Programadores fazem isso para marcar eventos, medir tempo ou apenas mostrar a data para os usuários.

## Como Fazer:
```C#
using System;

class Program
{
    static void Main()
    {
        DateTime hoje = DateTime.Now; // Pega a data e hora atuais
        Console.WriteLine(hoje.ToString("dd/MM/yyyy")); // imprime a data no formato dia/mês/ano
    }
}
/* Saída:
31/03/2023
*/
```

## Mergulho Profundo:
A classe `DateTime` existe desde as primeiras versões do .NET Framework e segue firme no .NET 5/6. Já o `DateTime.Now` é um clássico: te dá data e hora baseado no fuso do sistema. Há o irmão `DateTime.UtcNow` sem fuso – útil para padrões globais. 

Ah, e tem mais: se precisar só da data, sem a hora? Tá lá o `DateTime.Today`. Quer detalhes de implementação? `DateTime` é um struct, não uma class, representando data e hora com recursos de fuso horário limitados. Para fusos e datas mais complexas, explora `DateTimeOffset` e `TimeZoneInfo`.

Alternativas modernas? Sim! `System.Globalization` e bibliotecas como NodaTime gerenciam datas e horas de formas ainda mais precisas e diversificadas. Boa para não se perder entre fusos e calendários.

## Veja Também:
- [Documentação oficial DateTime](https://docs.microsoft.com/pt-pt/dotnet/api/system.datetime?view=net-6.0)
- [NodaTime, uma alternativa poderosa](https://nodatime.org/)
- [Globalização e localização em .NET](https://docs.microsoft.com/pt-pt/dotnet/standard/globalization-localization/)
