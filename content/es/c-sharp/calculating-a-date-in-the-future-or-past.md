---
title:                "C#: Calculando una fecha en el futuro o pasado."
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular la fecha en el futuro o pasado? 

Calcular la fecha en el futuro o pasado puede ser útil en diversas situaciones, como planificar eventos, verificar plazos de entrega o realizar proyecciones financieras. Con la programación en C#, podemos crear una función que nos permita calcular y obtener fechas precisas según nuestras necesidades.

## Cómo hacerlo

Primero, debemos importar la librería "System" en nuestro código:

```C#
using System;
```

A continuación, podemos utilizar el método "AddDays" para agregar una cantidad específica de días a una fecha determinada. Por ejemplo, si queremos calcular una fecha 10 días en el futuro, podemos hacer lo siguiente:

```C#
DateTime fechaActual = DateTime.Today;
DateTime fechaFutura = fechaActual.AddDays(10);
Console.WriteLine("La fecha actual es: " + fechaActual);
Console.WriteLine("La fecha en 10 días es: " + fechaFutura);
```

La salida de este código sería:

```
La fecha actual es: 15/04/2021
La fecha en 10 días es: 25/04/2021
```

También podemos utilizar el método "AddMonths" para agregar una cantidad de meses, o "AddYears" para agregar años a una fecha. Por ejemplo, si queremos calcular una fecha 6 meses en el pasado, podemos hacer lo siguiente:

```C#
DateTime fechaActual = DateTime.Today;
DateTime fechaPasada = fechaActual.AddMonths(-6);
Console.WriteLine("La fecha actual es: " + fechaActual);
Console.WriteLine("La fecha en 6 meses en el pasado es: " + fechaPasada);
```

La salida de este código sería:

```
La fecha actual es: 15/04/2021
La fecha en 6 meses en el pasado es: 15/10/2020
```

También es posible calcular una fecha en el futuro o pasado a partir de una fecha específica. Por ejemplo, si queremos saber qué día de la semana será dentro de 50 días a partir de una fecha dada, podemos hacer lo siguiente:

```C#
DateTime fechaEspecifica = new DateTime(2021, 05, 07);
DateTime fechaFutura = fechaEspecifica.AddDays(50);
Console.WriteLine("La fecha específica es: " + fechaEspecifica);
Console.WriteLine("La fecha en 50 días es: " + fechaFutura);
```

La salida de este código sería:

```
La fecha específica es: 07/05/2021
La fecha en 50 días es: 26/06/2021
```

## Profundizando más

Además de los métodos mencionados anteriormente, existen otras formas de calcular fechas en el futuro o pasado en C#. Podemos utilizar el método "Add" para sumar una cantidad específica de tiempo a una fecha determinada, por ejemplo:

```C#
DateTime fechaActual = DateTime.Today;
DateTime fechaFutura = fechaActual.Add(new TimeSpan(4, 6, 30));
Console.WriteLine("La fecha actual es: " + fechaActual);
Console.WriteLine("La fecha en 4 horas, 6 minutos y 30 segundos es: " + fechaFutura);
```

La salida de este código sería:

```
La fecha actual es: 15/04/2021
La fecha en 4 horas, 6 minutos y 30 segundos es: 15/04/2021 04:06:30
```

También podemos utilizar el método "Date" para obtener la fecha actual sin la hora, o el método "Now" para obtener la fecha y hora actuales. Podemos experimentar con estos y otros métodos para calcular fechas de la manera que mejor se adapte a nuestras necesidades en un proyecto.

## Ver también

- [Documentación de Microsoft para la clase DateTime en C#](https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0)
- [Ejemplos prácticos de cálculos de fechas en C#](https://www.geeksforgeeks.org/c-sharp-datetime-add-methods/)
- [Respuestas de la comunidad de Stack Overflow sobre cómo calcular fechas en C#](https://stackoverflow.com/questions/50879363/datetime-calculations-add-months-subtract-months-in-c-sharp)