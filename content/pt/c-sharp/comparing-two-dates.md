---
title:                "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Comparar duas datas pode ser uma tarefa muito útil em projetos de programação. Isso permite que os desenvolvedores possam verificar se uma data é anterior, posterior ou igual a outra. Além disso, essa funcionalidade pode ser utilizada em diversas áreas, desde cálculos financeiros até a organização de eventos.

## Como Fazer

Para comparar duas datas em C#, primeiro é necessário criar duas variáveis do tipo DateTime, que armazenam as datas que desejamos comparar. Em seguida, podemos utilizar o método Compare do DateTime para realizar a comparação e receber o resultado em um inteiro, onde:

- Se o valor retornado for menor que 0, significa que a primeira data é anterior à segunda data;
- Se o valor retornado for igual a 0, significa que as datas são iguais;
- Se o valor retornado for maior que 0, significa que a primeira data é posterior à segunda data.

Veja um exemplo de código abaixo com duas datas definidas e a utilização do método Compare para compará-las:

````C#
DateTime data1 = new DateTime(2021, 06, 01);
DateTime data2 = new DateTime(2021, 06, 15);

int resultado = DateTime.Compare(data1, data2);
Console.WriteLine(resultado);
````

O resultado obtido no exemplo acima será 14, pois a data1 é anterior à data2 em 14 dias.

## Deep Dive

Além do método Compare, existem outras formas de comparar datas em C#. Podemos utilizar os operadores lógicos, como "maior que" (>) ou "menor que" (<), para fazer a comparação diretamente entre as variáveis DateTime.

Outra opção é utilizar o método Equals para verificar se as datas são iguais ou o método CompareTo para obter um resultado semelhante ao método Compare.

Além disso, também é possível trabalhar com datas em diferentes formatos, como apenas ano, mês e dia, ou incluindo informações de hora, minuto e segundo. É importante atentar-se às diferenças entre datas locais e datas universais, bem como as diferentes culturas e formatos de datas utilizados.

## Veja Também

Para saber mais sobre como trabalhar com datas em C#, confira os links abaixo:

- [Manipulando Datas e Horas em C#](https://docs.microsoft.com/pt-br/dotnet/standard/datetime/)
- [Formatando Datas em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Diferenças entre Datas Locais e Universais em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/working-with-datetime)