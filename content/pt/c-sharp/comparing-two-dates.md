---
title:                "C#: Comparando duas datas"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Na programação, é comum precisarmos comparar duas datas diferentes. Pode ser para verificar se uma data é maior ou menor que a outra, ou até para identificar se elas são iguais. Por isso, é importante entender como fazer essa comparação de maneira eficiente em C#.

## Como fazer

Em C#, podemos usar o método `.Compare()` para comparar duas datas. Esse método retorna um inteiro indicando se as datas são iguais, ou se uma é maior ou menor que a outra.

Veja um exemplo de como usar esse método:

```C#
DateTime data1 = new DateTime(2021, 05, 12);
DateTime data2 = new DateTime(2021, 05, 20);

int resultado = data1.CompareTo(data2);

if (resultado == 0)
{
    // As datas são iguais
    Console.WriteLine("As datas são iguais");
}
else if (resultado == -1)
{
    // data1 é menor que data2
    Console.WriteLine("A data 1 é menor que a data 2");
}
else
{
    // data1 é maior que data2
    Console.WriteLine("A data 1 é maior que a data 2");
}

// Output: A data 1 é menor que a data 2
```

Podemos também usar os operadores de comparação `>` e `<` para comparar as datas. Esses operadores retornam um valor booleano (verdadeiro ou falso) indicando se a comparação é verdadeira ou falsa.

Veja o exemplo:

```C#
DateTime data1 = new DateTime(2021, 05, 12);
DateTime data2 = new DateTime(2021, 05, 20);

if (data1 < data2)
{
    // data1 é menor que data2
    Console.WriteLine("A data 1 é menor que a data 2");
}
else
{
    // data1 é maior ou igual a data2
    Console.WriteLine("A data 1 é maior ou igual a data 2");
}

// Output: A data 1 é menor que a data 2
```

## Deep Dive

Ao comparar duas datas, é importante levar em consideração não só o dia, mês e ano, mas também a hora, minutos e segundos. Isso pode ser feito especificando a precision desejada ao criar a instância da data.

Por exemplo:

```C#
DateTime data1 = new DateTime(2021, 05, 12);
DateTime data2 = new DateTime(2021, 05, 12, 15, 30, 0);

int resultado = data1.CompareTo(data2);

if (resultado == 0)
{
    // As datas são iguais
    Console.WriteLine("As datas são iguais");
}
else
{
    // As datas não são iguais
    Console.WriteLine("As datas não são iguais");
}

// Output: As datas não são iguais
```

Nesse caso, mesmo que os dias, mês e ano sejam iguais, a data1 é considerada menor que a data2, pois não foi especificada a hora, minutos e segundos.

## Veja também

- Documentação oficial da Microsoft: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.compare?view=net-5.0
- Tutorial completo sobre comparação de datas em C#: https://www.tutorialspoint.com/comparing-dates-in-c-sharp