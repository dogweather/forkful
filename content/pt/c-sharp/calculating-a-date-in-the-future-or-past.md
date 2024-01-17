---
title:                "Calculando uma data no futuro ou passado"
html_title:           "C#: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que e por que?

Calcular uma data no futuro ou no passado é uma tarefa comum na programação. Isso envolve a manipulação de datas e cálculos para determinar uma nova data baseada em uma data existente. Os programadores geralmente precisam fazer isso para automatizar tarefas, como agendar eventos, gerar relatórios com dados de datas específicas e muitos outros casos.

## Como fazer:

```C#
// Calcular uma data no futuro
DateTime dataAtual = DateTime.Today;
DateTime dataFutura = dataAtual.AddDays(30);

Console.WriteLine("A data atual é {0}", dataAtual);
Console.WriteLine("A data no futuro é {0}", dataFutura);

// Calcular uma data no passado
DateTime dataAtual = DateTime.Today;
DateTime dataPassada = dataAtual.AddYears(-5);

Console.WriteLine("A data atual é {0}", dataAtual);
Console.WriteLine("A data no passado é {0}", dataPassada);
```

Saída:
```
A data atual é 15/05/2021
A data no futuro é 14/06/2021

A data atual é 15/05/2021
A data no passado é 15/05/2016
```

## Profundando:

A necessidade de calcular datas no futuro ou no passado surgiu com o desenvolvimento dos primeiros calendários, onde era necessário determinar datas específicas para eventos e atividades. Com o avanço da tecnologia, surgiram diversas formas de fazer esses cálculos, como a adição ou subtração de dias, meses ou anos em uma data. Além disso, existem também bibliotecas e frameworks que facilitam esse processo, como o Noda Time e o Moment JS.

## Veja também:

- [Documentação oficial do C# sobre a classe DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Noda Time - Biblioteca para manipulação de datas em .NET](https://nodatime.org/)
- [Moment JS - Biblioteca para manipulação de datas em JavaScript](https://momentjs.com/)