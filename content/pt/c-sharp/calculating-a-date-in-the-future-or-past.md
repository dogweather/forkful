---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "C#: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Calcular uma data futura ou passada refere-se ao cálculo do dia, mês e ano após ou antes de um período de tempo especificado a partir da data atual. Programadores fazem isso para projetos que requeiram automatização de funções dependentes de datas, como renovação de contratos, envio de lembretes, entre outros.

## Como fazer:
Para calcular uma data futura ou passada, você pode usar o método `AddDays()`. Por exemplo:
```C#
 using System;

public class Program
{
    public static void Main()
    {
        DateTime dataAtual = DateTime.Now;
        Console.WriteLine("Data Atual: " + dataAtual);
        
        DateTime dataFutura = dataAtual.AddDays(7);
        Console.WriteLine("Data Futura: " + dataFutura);
        
        DateTime dataPassada = dataAtual.AddDays(-7);
        Console.WriteLine("Data Passada: " + dataPassada);
    }
}
```
No código acima, `AddDays(7)` adiciona 7 dias à data atual, e `AddDays(-7)` remove 7 dias da data atual. A saída do código será:
```C#
Data Atual: 25/09/2021 18:00:00
Data Futura: 02/10/2021 18:00:00
Data Passada: 18/09/2021 18:00:00
```
## Mergulho Profundo
Historicamente, os programadores precisavam calcular datas futuras e passadas manualmente, considerando anos bissextos, quantidade de dias em cada mês, etc. Mas, linguagens modernas como C# oferecem funções prontas para este propósito.

Existem outras alternativas para calcular datas futuras e passadas, como os métodos `AddHours()`, `AddMonths()`, etc.

Em relação aos detalhes de implementação, o método `AddDays()` trata do ano bissexto automaticamente. Ele também adiciona as horas, minutos, segundos e milésimos de segundo da data e hora atuais à nova data/hora.

## Veja também
Para detalhes adicionais e exemplos, você pode consultar as referências abaixo:
- [Documentação do Microsoft .Net](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.adddays?view=net-5.0)
- [Tutorial de DateTime no C#](https://www.tutorialsteacher.com/csharp/csharp-datetime)
- [Artigo Stackoverflow sobre datetime manipulation](https://stackoverflow.com/questions/3786616/how-to-deal-with-datetime-manipulation-in-c-sharp)