---
title:                "Obtendo a data atual"
html_title:           "C#: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Obter a data atual é uma tarefa comum para programadores em C#. Isso significa obter a data atual do sistema em que o programa está sendo executado. Essa informação é útil para muitos fins, como manter registros de datas de alterações ou exibir a data atual em um aplicativo.

## Como fazer:

Usando a classe `DateTime` e seu método `Now`, podemos obter facilmente a data atual em C#. Veja o exemplo abaixo:

```C#
DateTime dataAtual = DateTime.Now;
Console.WriteLine(dataAtual);
```
**Resultado:**
```
09/10/2021 22:14:05
```

## Profundidade:

A obtenção da data atual é uma tarefa muito comum em linguagens de programação e, portanto, há muitas maneiras de fazê-lo em C#. Uma alternativa seria usar a classe `DateTimeOffset`, que também fornece informações de fuso horário. Além disso, a classe `DateTime` possui métodos úteis para manipulação de datas, como `AddDays` e `ToString`.

## Veja também:

Para saber mais sobre a classe `DateTime` em C#, consulte a documentação oficial da Microsoft: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0.