---
title:    "C#: Obtendo a data atual"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual é importante

Ao desenvolver um programa, muitas vezes é necessário acessar o horário e data atuais. Isso pode ser útil para várias tarefas, como registrar a hora de uma transação ou criar um agendamento. Aprender a obter a data atual em um programa em C# pode ser extremamente útil em diversas situações.

## Como obter a data atual em C#

Para obter a data atual em C#, existem várias maneiras de realizar essa tarefa. Uma opção é utilizar a classe DateTime, que possui um método chamado Now. Esse método retorna a data e hora atuais no formato correto. Veja o exemplo abaixo:

```C#
DateTime dataAtual = DateTime.Now;
```

Se você quiser formatar a saída da data de uma maneira específica, pode usar o método ToString e passar o formato desejado como parâmetro. Por exemplo:

```C#
Console.WriteLine(dataAtual.ToString("dd/MM/yyyy"));
```

Isso irá imprimir a data no formato dia/mês/ano, como por exemplo 22/06/2021.

## Aprofundando na obtenção da data atual

Além da classe DateTime, também é possível obter a data atual utilizando a classe DateTimeOffset, que é semelhante, mas permite especificar o fuso horário. Outra opção é utilizar a classe TimeZoneInfo, que permite acessar informações sobre diferentes fusos horários e converter a data atual para um fuso específico.

Também é importante mencionar que a classe DateTime possui vários métodos e propriedades que podem ser utilizados para manipular e formatar a data e hora. Vale a pena explorá-los para entender melhor como trabalhar com datas e horas em C#.

## Veja também

- [Documentação oficial do C# sobre a classe DateTime](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0)
- [Tutorial sobre como trabalhar com datas em C#](https://www.c-sharpcorner.com/blogs/using-datetime-with-c-sharp-c-sharp-programming)
- [Artigo sobre a classe TimeZoneInfo em C#](https://www.c-sharpcorner.com/article/getting-time-zone-information-in-c-sharp/)