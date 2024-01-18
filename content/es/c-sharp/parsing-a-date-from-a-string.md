---
title:                "Analizando una fecha de una cadena"
html_title:           "C#: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

¡Hola! Si eres un programador de C#, seguramente has escuchado la frase "parsing a date from a string" muchas veces. Pero, ¿qué significa realmente y por qué lo hacemos? En este artículo, exploramos cómo hacerlo y hablamos un poco más sobre este tema.

## ¿Qué & Por qué?

El proceso de "parsing a date from a string" se refiere a tomar una fecha escrita en formato de cadena de texto y convertirla en un tipo de dato fecha legible para la computadora. Los programadores hacemos esto para poder manejar y utilizar fechas en nuestra código de forma más fácil y precisa.

## Cómo hacerlo:

Para parsear una fecha desde una cadena de texto en C#, utilizamos el método `DateTime.ParseExact()`. Este método toma dos argumentos: la cadena de texto con la fecha, y un patrón que le indica cómo está formateada la fecha en la cadena. Por ejemplo:

```C#
string fecha = "10/24/2021";
DateTime fechaParseada = DateTime.ParseExact(fecha, "MM/dd/yyyy", null);
```

En este ejemplo, utilizamos el patrón "MM/dd/yyyy" para indicar que la fecha está en formato de mes/día/año. También podríamos especificar un patrón diferente, dependiendo del formato en el que se encuentre la fecha en la cadena.

Una vez que tenemos nuestra fecha parseada, podemos utilizarla en nuestro código como cualquier otra fecha. Por ejemplo, podemos imprimir un mensaje con la fecha en un formato específico:

```C#
Console.WriteLine(fechaParseada.ToString("dddd, dd MMMM yyyy"));
```

Este código imprimirá la fecha en un formato más legible, como "domingo, 24 octubre 2021".

## Inmersión Profunda:

Parsing de fechas desde cadenas de texto ha existido por mucho tiempo en el mundo de la programación. Antes de utilizar el método `DateTime.ParseExact()`, solíamos utilizar la clase `DateTime.Parse()` que, si bien funciona de manera similar, no te permite especificar un patrón. Por lo tanto, `DateTime.ParseExact()` es una opción más precisa y robusta.

Además, existen otras opciones para parsear fechas en C#, como utilizar la clase `DateTimeOffset`, que incluye la zona horaria, o utilizar la librería externa NodaTime, que ofrece más flexibilidad y opciones de formato.

## Ver También:

Si quieres aprender más sobre el parseo de fechas en C#, aquí hay algunos recursos útiles:

- Documentación oficial de Microsoft sobre `DateTime.ParseExact()`: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact
- Tutorial en C# Corner: https://www.c-sharpcorner.com/article/c-sharp-datetime-parsing-examples/
- Documentación de NodaTime: https://nodatime.org/