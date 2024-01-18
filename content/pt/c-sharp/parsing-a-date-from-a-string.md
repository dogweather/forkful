---
title:                "Analisando uma data de uma string."
html_title:           "C#: Analisando uma data de uma string."
simple_title:         "Analisando uma data de uma string."
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que e Por Que?
A conversão de uma data de uma string é um processo comum na programação que envolve a transformação de uma data em formato de texto (string) em um formato de data reconhecido pelo sistema. Os programadores fazem isso para facilitar a manipulação e comparação de datas em seu código.

## Como Fazer:
```
// Exemplo 1: Converter uma string em data
string dataString = "01/01/2021";
DateTime data = DateTime.Parse(dataString);
Console.WriteLine(data); // Output: 01/01/2021 00:00:00

// Exemplo 2: Converter uma string em data com formato específico
string dataString = "01-01-2021";
DateTime data = DateTime.ParseExact(dataString, "dd-MM-yyyy", null);
Console.WriteLine(data); // Output: 01/01/2021 00:00:00
```

## Profundando:
A conversão de datas de strings tem sido uma tarefa importante na programação desde os primeiros dias dos sistemas de computador. Com o aumento do uso de diferentes formatos de data em todo o mundo, surgiram várias ferramentas e métodos para facilitar a conversão de datas em strings e vice-versa. Além da função `DateTime.Parse ()`, os programadores também podem usar o método `DateTime.TryParse ()` para validar a data antes da conversão.

## Veja Também:
- Documentação oficial da função `DateTime.Parse ()` em C#: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.parse?view=netcore-3.1
- Artigo sobre conversão de datas em strings em C#: https://www.educative.io/edpresso/how-to-parse-a-date-in-c-sharp