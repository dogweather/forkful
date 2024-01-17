---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "C#: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Qué y por qué?

Calcular una fecha en el futuro o en el pasado es un proceso común en la programación que implica usar una fecha inicial y un número de días, meses o años para obtener una nueva fecha. Los programadores utilizan esta funcionalidad para realizar cálculos y manipular fechas en sus aplicaciones.

Cómo:

```C#
var fechaInicial = new DateTime(2021, 6, 30);
var fechaFutura = fechaInicial.AddMonths(3);

Console.WriteLine($"La nueva fecha es {fechaFutura}."); // Output: 30/09/2021
```

```C#
var fechaInicial = new DateTime(2021, 6, 30);
var fechaPasada = fechaInicial.AddYears(-5);

Console.WriteLine($"La nueva fecha es {fechaPasada}."); // Output: 30/06/2016
```

Hundimiento Profundo:

Calcular fechas en el futuro o en el pasado es una tarea que ha existido desde los primeros días de la informática. En los primeros lenguajes de programación, era común interactuar con fechas y horas utilizando operaciones aritméticas básicas, lo que a menudo resultaba en cálculos complejos y propensos a errores. Con el tiempo, se han creado funciones y métodos más específicos para trabajar con fechas, facilitando a los programadores la realización de operaciones de fecha precisas y sin errores.

Alternativas:

Además de la función `Add` utilizada en los ejemplos anteriores, existen otras formas de calcular fechas en el futuro o en el pasado en C#. Por ejemplo, `AddDays`, `AddHours`, `AddSeconds` y `AddTicks` también permiten sumar una cantidad específica de tiempo a una fecha. Sin embargo, es importante tener en cuenta que la librería `DateTime` solo es precisa hasta los milisegundos, por lo que no se pueden sumar fracciones de segundo utilizando estas funciones.

Ver también:

- [Función Add de DateTime en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.datetime.add?view=net-5.0)
- [Métodos de DateTime en C# en el sitio web Codecademy](https://www.codecademy.com/resources/docs/csharp/date-and-time/datetime-methods-in-csharp)