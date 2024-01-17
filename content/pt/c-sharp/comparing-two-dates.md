---
title:                "Comparando duas datas."
html_title:           "C#: Comparando duas datas."
simple_title:         "Comparando duas datas."
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que é e por que os programadores fazem isso?

Comparar duas datas é um processo importante na programação, que envolve verificar se duas datas são iguais, anteriores ou posteriores. Os programadores geralmente fazem isso para garantir que as informações estejam sendo manipuladas corretamente e que as decisões baseadas em datas sejam precisas.

## Como fazer:

O C # possui uma classe embutida chamada DateTime que pode ser usada para comparar duas datas. Podemos usar os seguintes métodos para realizar a comparação:

```
DateTime data1 = new DateTime(2020, 10, 15);
DateTime data2 = new DateTime(2020, 10, 20);

// Verificando se as datas são iguais
if (data1 == data2)
{
    Console.WriteLine("As datas são iguais!");
}

// Verificando se a data1 é anterior à data2
if (data1 < data2)
{
    Console.WriteLine("Data1 é anterior à data2!");
}

// Verificando se a data1 é posterior à data2
if (data1 > data2)
{
    Console.WriteLine("Data1 é posterior à data2!");
}
```

A saída do código acima seria:

```
Data1 é anterior à data2!
```

## Mais detalhes:

A comparação de datas é uma parte importante da programação porque datas são frequentemente usadas para tomada de decisões em sistemas. Por exemplo, um sistema de reservas de hotel pode usar a comparação de datas para verificar a disponibilidade de quartos em uma data específica. Em vez de usar o método de comparação, também é possível usar a classe TimeSpan para calcular a diferença entre duas datas.

## Veja também:

- [Documentação da classe DateTime no C#](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=netcore-3.1)
- [Tutoriais de C# no site oficial da Microsoft](https://docs.microsoft.com/pt-br/dotnet/csharp/)
- [Artigo sobre comparação de datas em Java](https://www.devmedia.com.br/comparacao-de-datas-em-java/34887) (em português)