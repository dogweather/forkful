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

## Por qué

Calcular fechas futuras o pasadas puede ser muy útil en proyectos de programación, ya sea para crear un calendario, planificar eventos o realizar tareas de programación automatizadas.

## Cómo hacerlo

En C#, hay varias formas de calcular una fecha en el futuro o en el pasado. A continuación, te mostraremos algunos ejemplos utilizando la clase "DateTime".

```C#
// Obtener la fecha actual
DateTime fechaActual = DateTime.Now;

// Calcular una fecha en el futuro (1 año después)
DateTime fechaFutura = fechaActual.AddYears(1);

// Calcular una fecha en el pasado (1 mes antes)
DateTime fechaPasada = fechaActual.AddMonths(-1);

// Imprimir las fechas resultantes en la consola
Console.WriteLine($"Fecha futura: {fechaFutura}");
Console.WriteLine($"Fecha pasada: {fechaPasada}");

// Salida: 
// Fecha futura: 26/05/2022 19:35:14
// Fecha pasada: 26/03/2021 19:35:14
```

También puedes utilizar los métodos "AddDays", "AddHours", "AddMinutes" y "AddSeconds" para calcular fechas en el futuro o en el pasado sumando o restando cierta cantidad de días, horas, minutos o segundos.

```C#
// Calcular una fecha en el futuro (2 semanas después)
DateTime fechaFutura = fechaActual.AddDays(14);

// Calcular una fecha en el pasado (3 horas antes)
DateTime fechaPasada = fechaActual.AddHours(-3);

// Imprimir las fechas resultantes en la consola
Console.WriteLine($"Fecha futura: {fechaFutura}");
Console.WriteLine($"Fecha pasada: {fechaPasada}");

// Salida: 
// Fecha futura: 02/06/2021 19:35:14
// Fecha pasada: 26/05/2021 16:35:14
```

## Profundizando

La clase "DateTime" en C# también cuenta con otros métodos útiles para trabajar con fechas, como "Compare", "Equals", "DaysInMonth", entre otros. Además, también puedes utilizar métodos de formato para personalizar la apariencia de la fecha resultante. Por ejemplo:

```C#
// Obtener la fecha actual
DateTime fechaActual = DateTime.Now;

// Añadir 3 meses a la fecha actual
DateTime fechaFutura = fechaActual.AddMonths(3);

// Formatear la fecha resultante al formato "dd/mm/yy"
Console.WriteLine($"Fecha futura formateada: {fechaFutura.ToString("dd/MM/yy")}");

// Salida: Fecha futura formateada: 26/08/21
```

Con esto, puedes realizar cálculos y manipulaciones de fechas de una forma sencilla y eficiente en C#.

## Ver también

- [Documentación oficial de Microsoft sobre la clase DateTime en C# (en inglés)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Tutorial de CodeAcademy sobre trabajar con fechas en C# (en español)](https://www.codecademy.com/articles/datetime-csharp)