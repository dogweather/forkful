---
title:    "C#: Obteniendo la fecha actual"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

Obtener la fecha actual puede ser una tarea simple en C#, pero es una habilidad útil para cualquier programador. Puede ayudar a registrar eventos, generar nombres de archivo o simplemente mostrar la fecha actual en una aplicación. En esta publicación, exploraremos cómo obtener la fecha actual en C#.

## Cómo hacerlo

Para obtener la fecha actual en C#, utilizamos la clase DateTime. Esta clase contiene métodos y propiedades útiles para trabajar con fechas y horas.

```C#
// Importamos la clase DateTime
using System;

// Creamos una instancia de DateTime con la fecha actual
DateTime currentDate = DateTime.Now;

// Imprimimos la fecha actual en la consola
Console.WriteLine("La fecha actual es: " + currentDate);
```

El resultado de este código será:

```
La fecha actual es: [fecha actual]
```

También podemos formatear la fecha de diferentes maneras utilizando el método ToString(). Por ejemplo, si queremos mostrar solo el año y el mes, podemos hacerlo de la siguiente manera:

```C#
// Formateamos la fecha para mostrar solo el año y el mes
string formattedDate = currentDate.ToString("yyyy/MM");

// Imprimimos la fecha formateada en la consola
Console.WriteLine("El año y mes actual es: " + formattedDate);
```

El resultado de este código será:

```
El año y mes actual es: [año actual]/[mes actual]
```

También podemos utilizar otros parámetros en el método ToString() para formatear la fecha según nuestras necesidades.

## Profundizando

Además del método Now, la clase DateTime también contiene otros métodos útiles para obtener diferentes tipos de fechas y horas, como Today, que devuelve la fecha actual pero sin la hora, o UtcNow, que devuelve la hora actual en formato UTC.

También podemos utilizar la propiedad DayOfWeek para obtener el día de la semana de la fecha actual, o la propiedad DayOfYear para obtener el día del año actual.

La clase DateTime también incluye métodos para comparar fechas, sumar o restar días, meses o años, y mucho más. Puedes consultar la documentación oficial de Microsoft para obtener más información sobre todas las funcionalidades de la clase DateTime.

## Ver también

- [Clase DateTime en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0)
- [Tutoriales de programación en c# en español](https://www.tutorialesprogramacionya.com/csharpya/)
- [Stack Overflow, comunidad de programadores en español](https://es.stackoverflow.com/)