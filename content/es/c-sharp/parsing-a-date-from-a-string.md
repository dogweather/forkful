---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:35:32.220606-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Parsear una fecha desde un string significa convertir texto en un formato de fecha reconocible por el programa. Los desarrolladores hacen esto para manipular y almacenar fechas de manera consistente, a menudo provenientes de entradas de usuario o archivos externos.

## Cómo hacerlo:
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string fechaTexto = "24/03/2023";
        DateTime fecha;

        // Parsear una fecha simple
        fecha = DateTime.Parse(fechaTexto, new CultureInfo("es-ES"));
        Console.WriteLine(fecha);  // Salida: 24/03/2023 00:00:00

        // Parsear usando TryParse para evitar excepciones
        if(DateTime.TryParse(fechaTexto, out fecha))
        {
            Console.WriteLine(fecha);  // Salida: 24/03/2023 00:00:00
        }
        else
        {
            Console.WriteLine("Formato de fecha no válido.");
        }

        // Parsear con formato exacto
        fecha = DateTime.ParseExact(fechaTexto, "dd/MM/yyyy", CultureInfo.InvariantCulture);
        Console.WriteLine(fecha);  // Salida: 24/03/2023 00:00:00
    }
}
```

## Detalles profundos:
Parsear fechas es un problema antiguo en programación debido a los diferentes formatos usados alrededor del mundo (como DD/MM/AAAA vs MM/DD/AAAA). En C#, `DateTime.Parse()` y `DateTime.TryParse()` son métodos comunes para este propósito. Sin embargo, fallan si el formato del string no coincide con el formato esperado del sistema o si se dan entradas inválidas. Para evitar esto y tener un control más estricto, `DateTime.ParseExact()` y `DateTime.TryParseExact()` existen, permitiendo desarrolladores definir el formato exacto.

Antes de .NET, Visual Basic y otros lenguajes ofrecían sus propias maneras de parsear fechas, pero no tan sofisticadas. Al manipular fechas, los programadores deben ser conscientes de los problemas de localización y husos horarios. Librerías como NodaTime ofrecen alternativas con más funciones a las capacidades de fecha/hora en .NET.

## Ver también:
- Documentación oficial de Microsoft para `DateTime.Parse()`: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-7.0)
- Documentación oficial de Microsoft para `DateTime.ParseExact()`: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=net-7.0)
- NodaTime, una alternativa para manejo de fechas y horas: [NodaTime](https://nodatime.org/)
- Globalización y localización en .NET: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)
