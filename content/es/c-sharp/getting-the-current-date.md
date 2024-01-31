---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:13:31.112785-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en C# significa capturar el momento presente en tu programa. Los programadores hacen esto para registros de tiempo, marcas de fecha/hora y funciones dependientes del tiempo real.

## Cómo hacerlo:
```C#
using System;

class Program
{
    static void Main()
    {
        DateTime fechaActual = DateTime.Now;
        Console.WriteLine(fechaActual.ToString());
        
        // Salida: 4/5/2023 10:45:52 AM (variará según el momento exacto de ejecución)
    }
}
```

## Inmersión Profunda:
El tipo `DateTime` en .NET es un clásico para manejar fechas y horas. Antes del .NET, era común en lenguajes como VB utilizar funciones como `Now`. Hoy, en C#, `DateTime.Now` es la forma estándar de obtener la fecha y hora actuales del sistema. Además, puedes usar `DateTime.UtcNow` para la hora universal coordinada (UTC) o `DateTime.Today` si solo necesitas la fecha sin la hora. Internamente, `DateTime` representa ticks (100 nanosegundos) desde la medianoche del 1 de enero de 0001. Para contextos donde la precisión es crítica, .NET 6 introdujo `DateOnly` y `TimeOnly`.

## Ver También:
- [Documentación oficial de DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [TimeZoneInfo para manejar zonas horarias](https://docs.microsoft.com/en-us/dotnet/api/system.timezoneinfo?view=net-6.0)
- [NodaTime, biblioteca alternativa de fechas y horas](https://nodatime.org/)
