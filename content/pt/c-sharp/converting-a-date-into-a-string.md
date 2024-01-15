---
title:                "Convertendo uma data em uma string"
html_title:           "C#: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com dados e informações no C#, muitas vezes é necessário converter um objeto de data para uma string legível. Isso pode ser útil, por exemplo, quando se deseja exibir a data em um formato específico ou quando se precisa comparar duas datas.

## Como fazer

Para converter um objeto de data em uma string no C#, podemos usar o método `ToString()` da classe `DateTime`. Esse método permite especificar um formato de data e hora desejado, usando códigos de formato para representar dia, mês, ano, hora, entre outros.

Por exemplo, se quisermos exibir a data atual no formato "dd/MM/yyyy", podemos usar o seguinte código:

```
DateTime dataAtual = DateTime.Now;

string dataAtualString = dataAtual.ToString("dd/MM/yyyy");

Console.WriteLine(dataAtualString); // saída: 14/10/2021
```

Além disso, podemos adicionar formatos de data e hora personalizados, como apresentar a data na forma abreviada (dd/MM/yy) ou incluir o dia da semana (ddd, dd/MM/yyyy). Para isso, basta adicionar os códigos de formato desejados no parâmetro do método `ToString()`.

## Aprofundando-se

Além dos códigos de formato padrão, existem outros métodos que nos permitem converter uma data em uma string no C#. Por exemplo, o método `ToShortDateString()` retorna apenas a data no formato padrão da cultura atual do sistema operacional, enquanto o `ToShortTimeString()` retorna apenas a hora.

Também é possível personalizar ainda mais a exibição da data, utilizando a classe `DateTimeFormatInfo` para definir a cultura (idioma e região) desejada e seus padrões de formatação. Com isso, podemos exibir a data em diferentes idiomas ou em diferentes formatos de data.

## Veja também

- [Documentação da classe `DateTime`](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0)
- [Guia de formatação de data e hora no C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings)