---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:35:26.030855-07:00
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Converter uma data de texto para um formato de data permite manipulá-la com precisão no código. Programadores fazem isso para validar, armazenar ou comparar datas de maneira confiável.

## Como fazer:
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dataTexto = "31/01/2023";
        DateTime dataConvertida;

        try
        {
            dataConvertida = DateTime.ParseExact(dataTexto, 
                                                 "dd/MM/yyyy", 
                                                 CultureInfo.InvariantCulture);
            Console.WriteLine(dataConvertida.ToString("dd-MM-yyyy")); // Saída: 31-01-2023
        }
        catch (FormatException)
        {
            Console.WriteLine("Formato de data inválido.");
        }
    }
}
```
## Mergulho Profundo:
Converter datas de strings começou quando se precisava uma ponte entre a representação humana de datas e a representação computacional. Historiamente, era comum ver variações de implementação e formatos. Hoje, as coisas são mais padronizadas graças a convenções de cultura (como ISO 8601).

Alternativas incluem `DateTime.TryParse`, que é menos estrita que `ParseExact` e não lança exceção em caso de falha. Ainda temos o `DateTimeOffset` para lidar com fusos horários e o `CultureInfo` para tratar variações de formato de data/região.

Num nível de implementação, ao lidar com `ParseExact`, você especifica exatamente o formato esperado da string. Se o dado de entrada não corresponder exatamente a este formato, uma exceção é lançada. Por isso, é uma boa prática envolver o código em um bloco `try-catch` para tratar qualquer possível erro.

## Veja Também:
- Documentação oficial do `DateTime.ParseExact`: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.parseexact
- Diferenças entre `Parse` e `TryParse`: https://docs.microsoft.com/pt-br/dotnet/standard/base-types/parsing-datetime
- Guia de `DateTime` e `CultureInfo`: https://docs.microsoft.com/pt-br/dotnet/standard/globalization-localization/globalization
- Formato de data ISO 8601: https://www.iso.org/iso-8601-date-and-time-format.html
