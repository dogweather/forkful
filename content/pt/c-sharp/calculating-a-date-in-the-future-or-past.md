---
title:                "C#: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular uma data no futuro ou no passado pode ser útil em várias situações, como programar lembretes, agendamentos ou para obter informações em relatórios. Além disso, é uma habilidade importante para ter em sua caixa de ferramentas de programação.

## Como Fazer

Para calcular uma data no futuro ou no passado em C#, temos que seguir alguns passos simples:

1. Primeiro, precisamos criar uma instância da classe `DateTime` com a data atual como base. Isso pode ser feito da seguinte maneira:

```C#
DateTime dataAtual = DateTime.Now;
```

2. Depois, podemos usar o método `Add()` para adicionar ou subtrair um determinado número de dias, meses ou anos da data atual. Por exemplo, para calcular uma data 30 dias no futuro, podemos fazer o seguinte:

```C#
DateTime dataFutura = dataAtual.Add(new TimeSpan(30, 0, 0, 0));
```

3. Também é possível ajustar a data baseada em unidades menores, como horas, minutos e segundos. Por exemplo, para calcular uma data 12 horas e 30 minutos no passado, podemos usar o seguinte código:

```C#
DateTime dataPassada = dataAtual.Subtract(new TimeSpan(0, 12, 30, 0));
```

4. Por fim, podemos imprimir os resultados utilizando o método `ToString()` e especificando o formato de data desejado. Por exemplo:

```C#
Console.WriteLine(dataFutura.ToString("dd/MM/yyyy"));
Console.WriteLine(dataPassada.ToString("HH:mm:ss"));
```

Isso irá resultar em uma saída como esta:

```
03/01/2020
14:30:00
```

## Deep Dive

Além do método `Add()` e `Subtract()` que vimos anteriormente, existem outros recursos que podem ser úteis para calcular datas no futuro ou no passado em C#. Vamos ver alguns deles:

- É possível utilizar a classe `TimeSpan` para especificar o intervalo de tempo que queremos adicionar ou subtrair. Por exemplo, podemos passar um parâmetro `TimeSpan.FromDays()` para adicionar ou subtrair dias, `TimeSpan.FromHours()` para horas, etc.

- Também podemos utilizar o método `AddDays()`, `AddHours()`, etc. diretamente na instância da classe `DateTime` para evitar a criação de um objeto `TimeSpan`. Por exemplo, `dataAtual.AddDays(5)` irá adicionar 5 dias à data atual.

- Para obter o dia da semana de uma data calculada, podemos usar o método `DayOfWeek` da classe `DateTime`.

- Por fim, é importante lembrar que o formato de data pode variar de acordo com a cultura e o idioma do sistema operacional em que o código está sendo executado. Por isso, é recomendado utilizar o método `ToString()` com um especificador de formato para garantir a consistência dos resultados.

## Veja Também

- [Documentação oficial do .NET sobre a classe `DateTime`](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime)
- [Tutorial sobre o método `Add()` e `Subtract()` em C#](https://www.educative.io/edpresso/how-to-add-and-subtract-dates-in-c-sharp)
- [Guia completo sobre formatação de datas em C#](https://www.c-sharpcorner.com/article/date-and-time-format-in-c-sharp/)