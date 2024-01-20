---
title:                "Calcular una fecha en el futuro o en el pasado"
html_title:           "C#: Calcular una fecha en el futuro o en el pasado"
simple_title:         "Calcular una fecha en el futuro o en el pasado"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Calcular una fecha en el futuro o en el pasado significa encontrar exactamente qué día será o fue después o antes de un cierto período de tiempo. Los programadores hacen esto para programar recordatorios, calcular el tiempo de validez de contraseñas, y para muchas otras funcionalidades importantes.

## Cómo hacerlo:
La forma más sencilla de calcular una fecha futura o pasada en C# es usando la propiedad `AddDays` de la clase `DateTime`. Aquí está cómo hacerlo:

```C#
// Fecha actual
DateTime hoy = DateTime.Now;
Console.WriteLine("Hoy: {0}", hoy);

// Añadir 10 días
DateTime futuraFecha = hoy.AddDays(10);
Console.WriteLine("10 días después: {0}", futuraFecha);

// Restar 10 días
DateTime fechaPasada = hoy.AddDays(-10);
Console.WriteLine("10 días antes: {0}", fechaPasada);
```
Salida de muestra:
```
Hoy: 01/01/2022 12:00:00
10 días después: 11/01/2022 12:00:00
10 días antes: 22/12/2021 12:00:00
```

## Vaciamiento profundo
Históricamente, antes de la aparición de métodos simples como `AddDays`, los programadores tenían que lidiar con los años bisiestos y otros detalles de fechas manualmente. Además, la manipulación de fechas en C# no se limita a `AddDays`. Otras propiedades útiles son `AddMonths` y `AddYears`.

Alternativamente, puedes usar `TimeSpan` para operar con fechas. Esto es particularmente útil cuando necesitas precisión hasta el segundo o el milisegundo.

Uno de los aspectos más importantes de la manipulación de fechas es tener en cuenta las zonas horarias. `DateTimeOffset` es una clase en C# que te ayuda a manejar zonas horarias.

## Ver También
1. Documentación oficial de Microsoft .NET: [[Enlace]](https://docs.microsoft.com/es-es/dotnet/api/system.datetime.adddays?view=net-5.0)
2. Foro de discusión StackOverflow: [[Enlace]](https://stackoverflow.com/questions/3786612/c-sharp-datetime-subtraction)
3. Guía de Microsoft para la manipulación de fechas: [[Enlace]](https://docs.microsoft.com/es-es/dotnet/standard/datetime/)
4. Artículo de blog sobre el trabajo con fechas y horas en C#: [[Enlace]](https://code-maze.com/datetime-in-csharp/)