---
title:                "C#: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador em C# e está se perguntando por que alguém iria querer obter a data atual em seu código, este artigo é para você. A obtenção da data atual é um recurso muito útil para várias aplicações, desde a simples exibição da data em um programa até o cálculo do tempo decorrido entre duas datas.

## Como fazer

Para obter a data atual em C#, você pode utilizar o método `DateTime.Now`, que retorna um objeto do tipo `DateTime` representando a data e hora atuais. Veja abaixo um exemplo de código:

```C#
DateTime dataAtual = DateTime.Now;
Console.WriteLine("A data atual é: " + dataAtual);
```

A saída desse código será algo como "A data atual é: 28/10/2021 14:30:00".

Você também pode utilizar o método `DateTime.Today` para obter apenas a data atual sem a hora. Veja um exemplo:

```C#
DateTime dataAtual = DateTime.Today;
Console.WriteLine("Apenas a data atual é: " + dataAtual);
```

A saída será algo como "Apenas a data atual é: 28/10/2021 00:00:00".

Além disso, você pode utilizar outros métodos e propriedades da classe `DateTime` para formatar a data e hora de acordo com suas necessidades.

## Aprofundando-se

Ao utilizar os métodos `DateTime.Now` e `DateTime.Today`, é importante ter em mente que a data e hora retornadas serão baseadas no fuso horário do sistema. Caso queira obter a data e hora em um fuso horário específico, você pode utilizar o método `DateTime.UtcNow`, que retorna a data e hora em UTC (Tempo Universal Coordenado).

Além disso, é possível adicionar ou subtrair um período de tempo do objeto `DateTime` utilizando os métodos `Add` e `Subtract`, respectivamente. Por exemplo, se você quiser obter a data e hora daqui a 5 dias, pode fazer o seguinte:

```C#
DateTime dataAtual = DateTime.Now.Add(TimeSpan.FromDays(5));
```

## Veja também

- Documentação oficial da classe `DateTime` em C#: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime
- Como formatar datas e horas em C#: https://www.devmedia.com.br/ trabalhando-com-datas-e-horas-em-csharp/32593