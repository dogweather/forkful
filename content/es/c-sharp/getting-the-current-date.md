---
title:                "C#: Obteniendo la fecha actual"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un programador de C# y estás buscando una forma de obtener la fecha actual dentro de tu código, entonces este artículo es para ti. Obtener la fecha y hora actual es una tarea común en la programación, y en este post te enseñaremos diferentes formas de hacerlo con el lenguaje C#.

## Cómo hacerlo
Hay varias formas de obtener la fecha actual en C#, pero aquí te mostraremos dos métodos diferentes utilizando la clase "DateTime". Primero, puedes utilizar la propiedad "Now" para obtener la fecha y hora actual:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

La salida de este código será algo como: "10/13/2021 19:30:00". También puedes utilizar el método "Today" para obtener solo la fecha actual sin la hora:

```C#
DateTime currentDate = DateTime.Today;
Console.WriteLine(currentDate);
```

La salida de este código sería simplemente: "10/13/2021". Ahora, si deseas obtener la fecha o la hora por separado, puedes utilizar varios métodos de la clase "DateTime" como "Year", "Month", "Day", "Hour", "Minute", etc. Por ejemplo:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate.Year); // salida: 2021
Console.WriteLine(currentDate.Month); // salida: 10 (octubre)
Console.WriteLine(currentDate.Hour); // salida: 19 (7pm)
```

## Profundizando
Ahora que sabes cómo obtener la fecha y hora actual en C#, es importante entender cómo funciona en realidad la clase "DateTime". Esta clase es parte del espacio de nombres "System" y se utiliza para representar fechas y horas. Incluye métodos y propiedades para manipular fácilmente la fecha y la hora según tus necesidades.

Una cosa importante a mencionar es que la fecha y hora actuales pueden variar dependiendo de la zona horaria en la que se encuentre el sistema en el que se está ejecutando el código. Por lo tanto, si deseas obtener la fecha y hora precisas en una zona horaria específica, debes utilizar el método "UtcNow" en lugar de "Now". También puedes especificar la zona horaria usando el método "ToLocalTime".

En resumen, aprender a obtener la fecha y hora actual en C# es una habilidad útil y necesaria para muchos proyectos de programación. Ya sea que necesites mostrar la fecha en una aplicación, registrar eventos o realizar cálculos de tiempo, la clase "DateTime" te ayudará a hacerlo de manera sencilla y eficiente.

## Ver también
- [Clase DateTime en C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Métodos para obtener diferentes partes de la fecha y hora en C#](https://www.c-sharpcorner.com/article/how-to-get-current-date-time-in-c-sharp/)
- [Cómo trabajar con zonas horarias en C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tolocaltime?view=net-5.0)