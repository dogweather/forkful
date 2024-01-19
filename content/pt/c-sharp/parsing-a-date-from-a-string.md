---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Fazer parsing de uma data a partir de uma string é o processo de extrair e interpretar informação de data e hora de um texto. Programmers fazem isso para converter dados de texto, geralmente de entradas do usuário ou arquivos, em um formato de data útil para manipulação e cálculos.

## Como Fazer:

Aqui está um exemplo simples do código em C# para fazer parsing de uma data a partir de uma string usando `DateTime.TryParse`.

```c#
using System;

class Program
{
    static void Main()
    {
        string dataTexto = "2023/03/14"; // ano/mes/dia
        DateTime data;

        if (DateTime.TryParse(dataTexto, out data))
        {
            Console.WriteLine("Data: {0}", data);
        }
        else
        {
            Console.WriteLine("Formato de data inválido.");
        }
    }
}
```

Quando você executa este código, ele irá exibir "Data: 14/03/2023 00:00:00".

## Mergulho Profundo

Historicamente, parsing de datas de strings foi uma tarefa comum, mas complicada. Cada cultura tem sua maneira própria de representar datas, levando a uma variedade de formatos.

Como alternativa para `DateTime.TryParse`, você pode usar `DateTime.ParseExact` ou `DateTime.TryParseExact` se você souber o formato exato do texto da data.

No mencionado exemplo, `DateTime.TryParse` tenta converter a string fornecida em uma data. Se o parsing for bem sucedido, a variável `data` recebe o valor resultante. Se o parsing falhar, `data` é definido para 01/01/0001.

## Veja Também

Para uma visão mais detalhada do parsing de datas em C#, dê uma olhada nestes recurso:

- [Documentação oficial da Microsoft](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.tryparse?view=net-5.0)
- [Exemplos de código C# para parsing de date](https://www.delftstack.com/pt/howto/csharp/how-to-parse-date-in-csharp/)
- [Exemplos de parsing de data com TryParseExact](https://www.codingame.com/playgrounds/6179/c-tricks-2---date-and-time-parsing-using-tryparseexact)