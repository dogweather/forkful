---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparando Duas Datas em C#

## O Que e Por Que?

Comparar duas datas é verificar qual delas ocorre primeiro, depois ou se são iguais. Os programadores fazem isso frequentemente para organizar eventos, cronogramas e mais.

## Como Fazer:

Aqui estão alguns exemplos de como você pode comparar duas datas em C#:

```C#
DateTime data1 = new DateTime(2021, 07, 01);
DateTime data2 = new DateTime(2021, 08, 01);

int resultado = DateTime.Compare(data1, data2);

if(resultado < 0)
   Console.WriteLine("data1 é menor que data2.");
else if(resultado == 0)
   Console.WriteLine("data1 é igual a data2.");
else
   Console.WriteLine("data1 é maior que data2.");
```

Saída de amostra:

```C#
"data1 é menor que data2."
```

## Mergulho Profundo

Historicamente, o .NET fornece o método `DateTime.Compare` desde a sua primeira versão. Ainda hoje, é uma solução eficaz para comparar duas datas.

Existem outras maneiras de comparar duas datas em C#. Você pode subtrair uma data da outra e analisar o `TimeSpan` resultante. Ou simplesmente use os operadores de comparação (<, >, ==) diretamente.

Ao comparar as datas, o C# leva em consideração todos os componentes da data: o ano, o mês, o dia, a hora, o minuto, o segundo e até mesmo o milissegundo. Se você deseja apenas comparar partes de uma data (como apenas o dia, mês e ano), você precisa normalizar as datas para a mesma hora antes da comparação.

## Veja Também

- Documentação Microsoft para DateTime.Compare: https://msdn.microsoft.com/pt-br/library/system.datetime.compare(v=vs.110).aspx
- Comparando DateTime em C# : https://www.c-sharpcorner.com/blogs/comparing-datetime-in-c-sharp1
- Como Trabalhar com Datas e Horas em C#: https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/dates-times/