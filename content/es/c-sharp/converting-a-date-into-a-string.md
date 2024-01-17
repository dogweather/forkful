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

## ¿Qué y por qué?
La conversión de una fecha en una cadena es un proceso común en la programación en C#. Sirve para transformar los datos de una fecha en formato de texto para fines de visualización o almacenamiento. Los programadores realizan esta conversión para mostrar la fecha en un formato específico o para guardarla en una base de datos.

## Cómo hacerlo:
```C#
// Transformar una fecha en una cadena usando "ToString()"
DateTime fecha = new DateTime(2020, 05, 25);
string fechaCadena = fecha.ToString("dd/MM/yyyy"); // Salida: "25/05/2020"

// Transformar una fecha en una cadena personalizada usando "String.Format()"
DateTime fecha = new DateTime(2020, 05, 25);
string fechaCadena = String.Format("{0:MMM dd, yyyy}", fecha); // Salida: "May 25, 2020"

// Transformar una fecha actual en una cadena dinámica
string fechaActualCadena = DateTime.Now.ToString("F"); // Salida: "sábado, 16 de octubre de 2021 09:15:21 a.m."
```

## Profundizando
La conversión de una fecha en una cadena ha sido una función esencial en la programación durante años. Antes de los lenguajes de programación modernos, los programadores tenían que escribir manualmente el código para convertir las fechas en un formato comprensible para el usuario. Hoy en día, aparte de "ToString()" y "String.Format()", también existen otras opciones como "DateTime.ParseExact()" y librerías de terceros que facilitan esta tarea.

## Ver también
- [Cómo convertir una cadena en una fecha en C#](https://www.luisllamas.es/convertir-string-fecha-datetime-csharp/)
- [Documentación oficial de Microsoft sobre "DateTime.ToString()"](https://docs.microsoft.com/es-es/dotnet/api/system.datetime.tostring?view=net-5.0)
- [Otras opciones para trabajar con fechas en C#](https://codigoswan.com/mejores-formas-manejar-fechas-horas-csharp/)