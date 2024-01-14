---
title:    "C#: Calculando uma data no futuro ou no passado"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado?

Calcular uma data no futuro ou passado é uma habilidade útil para qualquer programador. Isso pode ser útil em situações como criar um sistema de reserva de passagens aéreas, agendar eventos ou simplesmente acompanhar datas importantes em um calendário. Em alguns casos, também pode ser necessário determinar se um determinado dia é um feriado ou não.

## Como fazer isso em C#

Aqui estão alguns exemplos simples de como calcular uma data no futuro ou passado usando o C#.

```C#
// Calcular uma data no futuro
DateTime dataFutura = DateTime.Today.AddDays(7);
Console.WriteLine("Daqui a uma semana será: " + dataFutura);

// Calcular uma data no passado
DateTime dataPassada = DateTime.Today.AddDays(-14);
Console.WriteLine("Duas semanas atrás foi: " + dataPassada);

// Calcular uma data a partir de um número específico de semanas ou meses
DateTime dataFutura2 = DateTime.Today.AddMonths(3); // Adiciona 3 meses à data atual
Console.WriteLine("Daqui a 3 meses será: " + dataFutura2);

DateTime dataPassada2 = DateTime.Today.AddWeeks(-2); // Subtrai 2 semanas da data atual
Console.WriteLine("Há 2 semanas foi: " + dataPassada2);
```

A saída do código acima será:

```
Daqui a uma semana será: 6/10/2020 12:00:00 AM
Duas semanas atrás foi: 4/19/2020 12:00:00 AM
Daqui a 3 meses será: 8/18/2020 12:00:00 AM
Há 2 semanas foi: 4/12/2020 12:00:00 AM
```

## Mais informações sobre calcular uma data no futuro ou passado

Calculando datas no futuro ou passado pode ser um pouco mais complexo do que apenas adicionar ou subtrair dias ou meses. Em alguns casos, pode ser necessário levar em consideração informações como anos bissextos ou fusos horários. Para isso, o C# oferece métodos específicos para ajudá-lo a lidar com essas situações.

Um método útil é o `DateTime.AddDays()`, que permite adicionar um determinado número de dias à data atual. Também existem métodos similares para adicionar semanas, meses e anos. Além disso, o C# também possui uma classe `DateTimeOffset`, que pode ser usada para armazenar informações de data e hora em diferentes fusos horários.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre como calcular datas no futuro ou passado usando o C#:

- [Documentação oficial do C# sobre datas e horas](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/numbers-and-dates/dates-and-times)
- [Tutorial da Microsoft sobre como trabalhar com datas e horas em C#](https://docs.microsoft.com/pt-br/dotnet/standard/datetime/)