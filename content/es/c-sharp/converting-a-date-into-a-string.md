---
title:    "C#: Convirtiendo una fecha en una cadena"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Por Qué

Convertingir fechas en cadenas de texto puede ser una habilidad útil en la programación C#. En lugar de mostrar la fecha en un formato predeterminado, convertirlo en una cadena de texto nos permite formatearla según nuestras necesidades y presentarla al usuario de una manera más legible.

##Cómo hacerlo

Para convertir una fecha en una cadena de texto, utilizamos la función `ToString()` y especificamos un formato en particular dentro de los paréntesis. Por ejemplo, si queremos mostrar la fecha con el formato "dd/MM/yyyy" (día/mes/año), utilizamos el siguiente código:

```C#
DateTime fecha = DateTime.Now;
string fechaTexto = fecha.ToString("dd/MM/yyyy");
Console.WriteLine(fechaTexto); // Salida: 10/03/2021
```

También podemos incluir información adicional en el formato, como el nombre del día o mes, las horas, minutos y segundos. Por ejemplo, si quisieramos mostrar la fecha y hora con el formato "dddd, dd MMMM yyyy - HH:mm:ss", utilizamos el siguiente código:

```C#
DateTime fecha = DateTime.Now;
string fechaTexto = fecha.ToString("dddd, dd MMMM yyyy - HH:mm:ss");
Console.WriteLine(fechaTexto); // Salida: miércoles, 10 marzo 2021 - 13:25:12
```

Incluso podemos utilizar caracteres especiales para personalizar aún más el formato, como en el siguiente ejemplo donde utilizamos el carácter "$" para indicar que la fecha está en formato español:

```C#
DateTime fecha = new DateTime(2020, 08, 25);
string fechaTexto = fecha.ToString("dddd, dd MMMM yyyy - $");
Console.WriteLine(fechaTexto); // Salida: martes, 25 agosto 2020 - 25 agosto 2020
```

##Profundizando

Al utilizar la función `ToString()` para convertir fechas en cadenas de texto, estamos utilizando un método de la clase `DateTime`, que representa una fecha y hora en C#. Este método nos permite especificar diferentes formatos para mostrar la fecha según nuestras necesidades.

Además, es importante tener en cuenta que la fecha debe estar en un formato válido antes de intentar convertirla en una cadena de texto. De lo contrario, se producirá un error en tiempo de ejecución.

##Ver También

- [Clase DateTime en Microsoft Docs](https://docs.microsoft.com/es-es/dotnet/api/system.datetime)
- [Cómo convertir una fecha a una cadena en C#](https://programacion.net/articulo/como_convertir_una_fecha_a_una_cadena_en_c__)
- [Cadenas de formato de fecha y hora en C#](https://docs.microsoft.com/es-es/dotnet/standard/base-types/custom-date-and-time-format-strings)