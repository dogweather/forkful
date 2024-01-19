---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Comparar dos fechas en programación implica determinar si una fecha es anterior, posterior o igual a la otra. Los programadores suelen realizar esto para tomar decisiones basadas en la relación temporal entre dos eventos.

## Cómo:
Aquí se muestra un código C# para comparar dos fechas:

```C#
using System;

class Program
{
    static void Main() 
    {
        DateTime fecha1 = new DateTime(2021, 9, 15);
        DateTime fecha2 = new DateTime(2022, 9, 15);

        int comparacion = DateTime.Compare(fecha1, fecha2);

        if (comparacion < 0) 
           Console.WriteLine("fecha1 es más temprano que fecha2.");
        else if (comparacion > 0)
           Console.WriteLine("fecha1 es más tarde que fecha2.");
        else
           Console.WriteLine("Ambas fechas son iguales.");
    }
}
```

Este es un posible resultado:

```
fecha1 es más temprano que fecha2.
```

## Inmersión Profunda:
Históricamente, la comparación de fechas ha sido crucial desde los primeros días de la programación. Es crucial en el modelado de eventos, la lógica de la aplicación, y los historiales de transacciones, entre otras muchas aplicaciones.

Una alternativa al método `DateTime.Compare()` es usar los operadores de comparación directamente, como `>` y `<`. Por ejemplo  `if (fecha1 > fecha2)`.
  
En cuanto a la implementación, `DateTime.Compare()` devolverá -1, 0 o 1 si la primera fecha es anterior, igual a, o posterior que la segunda respectivamente. Bajo el capó, el método convierte las fechas a marcas de tiempo para la comparación.

## Ver También:
1. [Documentación de DateTime en C#](https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0)
2. [Microsoft: Comparar dos fechas en C#](https://docs.microsoft.com/es-es/dotnet/api/system.datetime.compare?view=net-5.0)