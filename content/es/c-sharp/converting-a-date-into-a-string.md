---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "C#: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha a una cadena?

Existen varias razones por las cuales uno puede necesitar convertir una fecha en una cadena en C#. En primer lugar, puede ser necesario para mostrar la fecha en un formato específico en una interfaz de usuario. También puede ser útil para almacenar la fecha en una base de datos o para realizar comparaciones y operaciones matemáticas con fechas.

## Cómo hacerlo

Converting una fecha a una cadena en C# es fácil gracias a la clase `DateTime` y los métodos `ToString()` y `Format()`.

```C#
DateTime fecha = new DateTime(2020, 10, 15); // creando una fecha
string cadena1 = fecha.ToString(); // convirtiendo a cadena en formato predeterminado
Console.WriteLine(cadena1); // salida: 10/15/2020 12:00:00 AM

string cadena2 = fecha.ToString("dd/MM/yyyy"); // convirtiendo a cadena en formato personalizado
Console.WriteLine(cadena2); // salida: 15/10/2020

string cadena3 = fecha.ToString("MMMM dd, yyyy"); // convirtiendo a cadena con nombre del mes
Console.WriteLine(cadena3); // salida: October 15, 2020

string cadena4 = string.Format("Hoy es {0:dddd, MMMM d, yyyy}", fecha); // usando el método Format()
Console.WriteLine(cadena4); // salida: Hoy es Thursday, October 15, 2020
```

## Profundizando

La clase `DateTime` en C# proporciona múltiples métodos para convertir una fecha en una cadena en diferentes formatos. Los formatos comunes incluyen `"MM"` para el número del mes, `"MMMM"` para el nombre completo del mes y `"yy"` para los últimos dos dígitos del año. También se pueden usar combinaciones de estos formatos para crear una cadena personalizada.

Además, también se pueden utilizar métodos específicos para obtener partes individuales de la fecha, como `Year`, `Month` y `Day`, y luego combinarlos con una cadena para crear un formato a medida.

Es importante tener en cuenta que cuando se convierte una fecha a una cadena, también se puede especificar un idioma y una cultura para que la fecha y los nombres de los meses se muestren correctamente en diferentes regiones.

## Ver también

- Documentación de Microsoft para la clase DateTime en C#: https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=netcore-3.1
- Un tutorial detallado sobre cómo convertir fechas en cadenas en C#: https://www.tutorialspoint.com/csharp/csharp_date_time.htm
- Ejemplos prácticos de conversión de fechas a cadenas en diferentes formatos: https://www.c-sharpcorner.com/blogs/date-and-time-format-in-c-sharp-programming1