---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que e Porquê?

Converter uma data para uma string é o processo de transformar um objeto DateTime num formato de texto legível. Programadores fazem isso para facilitar a exibição, armazenamento ou transporte de datas através de sistemas que reconhecem texto.

## Como Fazer:

No C#, existem várias maneiras de converter uma data em uma string. Considere o seguinte exemplo, onde temos uma data atual (`DateTime.Now`) e queremos convertê-la em uma string:

```C#
DateTime dataAtual = DateTime.Now;
string strData = dataAtual.ToString("dd/MM/yyyy");

Console.WriteLine(strData);
```

A saída será:

```C#
"24/10/2021"
```

Muito simples, certo?

## Mergulho Profundo

Historicamente, a conversão de datas em strings é uma prática comum em muitas linguagens de programação, e C# não é exceção. Na verdade, C# possívelmente tem ainda mais opções devido à sua ampla biblioteca de métodos de string.

Há também alternativas ao método `ToString()`. Por exemplo, você pode usar `String.Format` ou interpolação de string (`$"`):

```C#
string strDataFormat = String.Format("{0:dd/MM/yyyy}", dataAtual);
Console.WriteLine(strDataFormat);

string strDataInterpol = $"{dataAtual:dd/MM/yyyy}";
Console.WriteLine(strDataInterpol);
```

Ambos produzem a mesma saída que `dataAtual.ToString("dd/MM/yyyy")`. A diferença está principalmente na sintaxe e na legibilidade, o que pode variar de acordo com a situação e a preferência pessoal.

## Veja Também

Aqui estão algumas fontes úteis para um aprofundamento maior no assunto:

- Documentação oficial da Microsoft sobre formatos de data e hora em C#: https://docs.microsoft.com/pt-br/dotnet/standard/base-types/standard-date-and-time-format-strings
- StackOverflow - Discussões sobre melhores práticas para converter datas em strings: https://stackoverflow.com/questions/12422623/best-way-to-convert-datetime-object-to-string-in-csharp