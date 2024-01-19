---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Obter a data atual é um processo básico em programação que retorna a data e a hora correntes. Isso é útil para rastrear eventos, registrar informações e controlar cronometragens em aplicações.

## Como Fazer:

Em C#, você pode obter a data atual usando a classe `DateTime` e a sua propriedade `Now`. Veja um exemplo simples a seguir:
```C#
using System;

public class Program
{
    public static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate);
    }
}
```
E a saída seria algo como:
```Text
10/8/2023 9:30:45 AM
```
## Aprofundamento:

1. Contexto Histórico: A classe `DateTime` foi introduzida no .NET Framework 1.0, tornando-se a forma padrão de trabalhar com datas e horas em C#.
2. Alternativas: Você pode usar `DateTime.UtcNow` para obter a data e hora GMT. Para maior precisão, os desenvolvedores agora podem usar a classe `DateTimeOffset`.
3. Detalhes de Implementação: A propriedade `Now` da classe `DateTime` obtém a data e hora do sistema do computador corrente.

## Veja Também:

O .NET possui muitas classes e métodos para trabalhar com datas e horas. Aqui estão alguns links úteis para obter mais informações:

- [Documentação Oficial da Classe DateTime](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-6.0)
- [Artigo no C# Corner sobre a Classe DateTime](https://www.c-sharpcorner.com/UploadFile/mahesh/working-with-datetime-in-C-Sharp/)
- [Artigo no StackOverflow sobre a diferença entre Now e UtcNow](https://stackoverflow.com/questions/6222291/when-would-one-use-datetime-now-over-datetime-utcnow)