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

##Por que Comparar Duas Datas?

Comparar duas datas é uma tarefa comum em programação, especialmente quando se lida com dados de tempo e agendamentos. É importante entender como comparar datas corretamente para garantir que as informações sejam precisas e consistentes.

##Como Fazer

A comparação de datas em C# pode ser feita usando o operador de comparação "maior que" (>) e "menor que" (<). Existem também métodos específicos da classe DateTime, como Compare e CompareTo, que retornam um valor numérico indicando a relação entre duas datas.

Veja alguns exemplos práticos de como comparar duas datas em C#:

```C#
// Com operadores de comparação
DateTime data1 = new DateTime(2020, 3, 15);
DateTime data2 = new DateTime(2020, 3, 20);

if (data1 > data2)
{
    Console.WriteLine("A data1 é maior que a data2");
}
else if (data1 < data2)
{
    Console.WriteLine("A data1 é menor que a data2");
}
else
{
    Console.WriteLine("As datas são iguais");
}

// Com o método Compare
DateTime data3 = new DateTime(2020, 3, 10);
DateTime data4 = new DateTime(2020, 3, 10);

int resultado = DateTime.Compare(data3, data4);

if (resultado > 0)
{
    Console.WriteLine("A data3 é maior que a data4");
}
else if (resultado < 0)
{
    Console.WriteLine("A data3 é menor que a data4");
}
else
{
    Console.WriteLine("As datas são iguais");
}

// Com o método CompareTo
DateTime data5 = new DateTime(2020, 3, 5);
DateTime data6 = new DateTime(2020, 3, 10);

int resultado2 = data5.CompareTo(data6);

if (resultado2 > 0)
{
    Console.WriteLine("A data5 é maior que a data6");
}
else if (resultado2 < 0)
{
    Console.WriteLine("A data5 é menor que a data6");
}
else
{
    Console.WriteLine("As datas são iguais");
}
```

A saída esperada para esses exemplos seria:

```
A data1 é menor que a data2
As datas são iguais
A data5 é menor que a data6
```

##Aprofundando

Ao comparar duas datas, é importante levar em consideração a parte horária (hora, minuto, segundo e milissegundo) das mesmas. Se ambos os objetos DateTime tiverem a mesma data, mas com horários diferentes, o resultado da comparação será influenciado por esse detalhe.

Outro ponto importante a se considerar é o uso de diferentes culturas (cultural settings) e fusos horários (time zones) na comparação de datas. Isso pode afetar o resultado da comparação, já que diferentes culturas podem ter diferentes formatos de data e fusos horários podem influenciar a hora atual.

Também é possível comparar apenas a data, ignorando a parte horária, usando os métodos Date ou DateOnly da classe DateTime.

A classe TimeSpan também pode ser útil ao comparar datas, permitindo a obtenção da diferença entre duas datas em diferentes unidades de tempo, como dias, horas, minutos ou segundos.

##Veja Também

- Documentação da classe DateTime em C#: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=netcore-3.1
- Guia completo para comparação de datas em C#: https://www.tutorialspoint.com/csharp/csharp_date_time.htm