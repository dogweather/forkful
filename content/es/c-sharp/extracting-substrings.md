---
title:    "C#: Extrayendo subcadenas"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad importante en la programación de C#. Esto permite a los programadores manipular cadenas de texto de manera más específica y eficiente, lo que puede ser útil en una variedad de situaciones, como la validación de entradas de usuario o la manipulación de datos en una base de datos.

## Cómo hacerlo

Primero, debemos entender qué es una subcadena. Una subcadena es simplemente un segmento de una cadena más larga, similar a cómo una palabra es un segmento de una oración. La sintaxis general para extraer una subcadena en C# es la siguiente:

```
string subcadena = texto.Substring(inicio, longitud);
```

Donde "texto" es la cadena de la que queremos extraer la subcadena, "inicio" es la posición inicial de la subcadena en la cadena original y "longitud" es la cantidad de caracteres que queremos extraer. Por ejemplo, si tenemos la cadena "Hola mundo" y queremos extraer la subcadena "mundo", usaríamos la siguiente línea de código:

```
string subcadena = "Hola mundo".Substring(5, 5);

// subcadena = "mundo"
```

También podemos usar métodos como "IndexOf" y "LastIndexOf" para encontrar la posición de una determinada subcadena o carácter en una cadena y luego usarlos junto con "Substring" para extraer la subcadena deseada.

## Un poco más profundo

Es importante recordar que las cadenas de texto son inmutables en C#, lo que significa que no se pueden cambiar después de que se crean. Esto significa que cada vez que extraemos una subcadena, se crea una nueva cadena en la memoria, por lo que es importante tener esto en cuenta para no desperdiciar recursos.

Otra cosa a tener en cuenta es que las posiciones de las cadenas comienzan en 0 en C#, por lo que la primera posición de una cadena es la posición 0, la segunda es la posición 1 y así sucesivamente.

## Ver también

- [Documentación de Microsoft sobre el método Substring] (https://docs.microsoft.com/es-es/dotnet/api/system.string.substring?view=netcore-3.1)
- [Cómo usar subcadenas en C#] (https://www.geeksforgeeks.org/c-sharp-extract-substring-from-a-specified-location-and-length-of-a-string/)
- [Tutorial de subcadenas en C#] (https://www.codegrepper.com/code-examples/csharp/c%23+substring)

¡Ahora estás listo para empezar a usar subcadenas en tus proyectos de C#! Con estas habilidades, podrás manipular cadenas de texto de manera más eficiente y específica. ¡Buena suerte!