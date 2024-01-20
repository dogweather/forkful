---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Obtener la fecha actual en la programación es un concepto sencillo pero muy útil que se refiere a capturar el día exacto en el que se ejecuta un programa. Los programadores lo hacen para funciones de registro, timestamping y muchas funciones de tiempo vitales.

## ¿Cómo hacer esto?

Podemos usar la clase DateTime para obtener la fecha actual en C#. Aquí tienes un ejemplo:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime fechaActual = DateTime.Now;
        Console.WriteLine("La fecha actual es: " + fechaActual);
    }
}
```

Cuando ejecutes este programa, verás algo como esto en la salida:

```C#
La fecha actual es: 12/9/2022 5:50:12 PM
```

## Información más profunda

La capacidad de obtener la fecha y hora actual en C# ha sido una característica desde la versión 1.0. A lo largo de los años, Microsoft ha añadido más funciones para dar más control sobre cómo obtener y mostrar la fecha y la hora.

Aunque el método `DateTime.Now` es la forma más común de obtener la fecha y la hora actuales, también existe `DateTime.UtcNow` que te proporciona la fecha y la hora universales coordinadas (UTC). Resaltar la diferencia entre ambos puede ser crucial para el manejo correcto de las zonas horarias en tu aplicación.

Las fechas y horas en C# se almacenan internamente como el número de "ticks" desde un punto fijo en el pasado. Un "tick" es una duración de 100 nanosegundos, y el punto de partida es la medianoche del 1 de enero del año 1.

## Ver También

Para obtener información más detallada y avanzada sobre las fechas y horas en C#, recomendaría las siguientes fuentes:

- [Fecha y hora en C#](https://docs.microsoft.com/es-es/dotnet/standard/datetime)
- [Formato de fecha y hora en C#](https://docs.microsoft.com/es-es/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Zonificación en C#](https://docs.microsoft.com/es-es/dotnet/standard/datetime/converting-between-time-zones)