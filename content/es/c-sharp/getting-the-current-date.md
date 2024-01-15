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

## Por qué

En programación, obtener la fecha y hora actuales es una tarea común y útil. Con la fecha y hora actuales, podemos realizar tareas como controlar la caducidad de una suscripción, mostrar la fecha de creación de un archivo o registro, o simplemente informar al usuario la hora actual.

## Cómo hacerlo

Para obtener la fecha y hora actuales en C#, debemos utilizar el método `DateTime.Now`. Este método devuelve un objeto `DateTime` que contiene la fecha y hora actuales. Veamos un ejemplo de cómo podemos utilizarlo:

```
using System;

namespace CurrentDateExample
{
    class Program
    {
        static void Main(string[] args)
        {
            DateTime currentDate = DateTime.Now;
            Console.WriteLine("La fecha actual es: " + currentDate.ToShortDateString());
            Console.WriteLine("La hora actual es: " + currentDate.ToShortTimeString());
        }
    }
}
```

El código anterior imprimirá la fecha y hora actuales en el formato corto (por ejemplo, 9/9/2021 y 3:30 PM). También podemos obtener la fecha y hora en otros formatos utilizando los métodos `ToString()` y `ToLongDateString()` de la clase `DateTime`.

## Profundizando

El método `DateTime.Now` utiliza el reloj del sistema para obtener la fecha y hora actuales. Si necesitamos obtener la fecha y hora en una zona horaria específica, podemos utilizar el método `DateTime.UtcNow` para obtener la fecha y hora en formato UTC sin la compensación del horario de verano.

También podemos utilizar la clase `DateTimeOffset` para obtener la fecha y hora con una zona horaria específica, lo que es útil para aplicaciones que operan en distintas zonas horarias.

## Ver también

- [Obtener la fecha y hora actuales en C#](https://docs.microsoft.com/es-es/dotnet/api/system.datetime.now)
- [DateTimeOffset en C#](https://docs.microsoft.com/es-es/dotnet/api/system.datetimeoffset)