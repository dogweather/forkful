---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "C#: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que?
Algumas vezes, em nossos códigos, precisamos calcular datas no futuro ou no passado. Isso pode ser necessário para realizar tarefas como agendamento de eventos, expiração de assinaturas ou validação de datas de nascimento. Felizmente, C# possui uma série de recursos que facilitam essa tarefa.

## Como fazer
Para calcular uma data no futuro ou no passado em C#, precisamos utilizar a classe DateTime e seus métodos. Vamos ver alguns exemplos práticos:

```
// Para obter a data atual
DateTime dataAtual = DateTime.Today;

// Para adicionar dias a uma data
DateTime dataFutura = dataAtual.AddDays(7); // Adiciona 7 dias à data atual

// Para subtrair dias de uma data
DateTime dataPassada = dataAtual.AddDays(-14); // Subtrai 14 dias da data atual

// Para adicionar anos ou meses a uma data
DateTime dataFutura = dataAtual.AddYears(1); // Adiciona 1 ano à data atual
DateTime dataTambemFutura = dataAtual.AddMonths(3); // Adiciona 3 meses à data atual
```

Outra forma de calcular datas no futuro ou no passado é utilizando o construtor da classe DateTime, passando os parâmetros para o ano, mês e dia desejados. Por exemplo:

```
// Para obter uma data no passado
DateTime dataPassada = new DateTime(2020, 2, 22);

// Para obter uma data no futuro
DateTime dataFutura = new DateTime(2022, 11, 15);
```

Vale ressaltar que é possível combinar a utilização destes métodos e construtores para obter resultados mais complexos. É importante também estar atento às propriedades da classe DateTime, como a propriedade DayOfWeek, que pode ser utilizada para obter o dia da semana de uma determinada data.

## Mergulho profundo
Além dos métodos e construtores mencionados anteriormente, existem outras formas de manipular datas no C#. Por exemplo, a classe TimeSpan pode ser utilizada para representar um intervalo de tempo, permitindo realizar cálculos mais precisos. Também é possível utilizar a classe Calendar ou culture-specific date and time formats para trabalhar com diferentes formatos de data.

Para mais informações sobre a manipulação de datas em C#, consulte a documentação oficial da Microsoft: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0

## Veja também
- https://www.devmedia.com.br/trabalhando-com-datas-e-horas-em-c/21661
- https://www.eduardopires.net.br/2013/03/criando-uma-aplicacao-que-trabalha-com-datas-em-csharp/