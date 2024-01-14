---
title:    "C#: Buscar y reemplazar texto"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

Muchas veces, al escribir código en C#, es necesario realizar cambios en una gran cantidad de texto. Esto puede ser tedioso y propenso a errores si se hace manualmente. Es por eso que es importante conocer cómo realizar búsquedas y reemplazos de texto de manera eficiente en C#.

## Cómo hacerlo

La forma más común de realizar búsquedas y reemplazos de texto en C# es utilizando el método `Replace()` en la clase `String`. Este método toma dos parámetros: el texto que se desea reemplazar y el texto con el que se desea reemplazarlo.

```C#
string texto = "Hola, mi nombre es Juan.";

Console.WriteLine(texto.Replace("Juan", "María"));

// Output: Hola, mi nombre es María.
```

También es posible utilizar expresiones regulares para realizar búsquedas y reemplazos más avanzados. Estas son patrones que representan un conjunto de cadenas de texto y permiten buscar y manipular texto con mayor precisión.

```C#
string texto = "El número de teléfono de María es 123456789.";

Console.WriteLine(Regex.Replace(texto, "[0-9]+", "XXXXXX"));

// Output: El número de teléfono de María es XXXXXX.
```

Otra forma de realizar búsquedas y reemplazos de texto es utilizando la clase `StringBuilder`. Esta clase permite construir y manipular cadenas de texto de manera eficiente.

```C#
StringBuilder texto = new StringBuilder("Mi nombre es Juan.");

texto.Replace("Juan", "María");

Console.WriteLine(texto.ToString());

// Output: Mi nombre es María.
```

Es importante tener en cuenta que tanto en el método `Replace()` como en las expresiones regulares y la clase `StringBuilder`, las búsquedas y reemplazos son sensibles a mayúsculas y minúsculas. Para realizar búsquedas y reemplazos sin importar esto, se pueden utilizar los métodos `ToLower()` y `ToUpper()` en la clase `String`.

## Profundizando

Además de las formas mencionadas anteriormente, existen otras formas de realizar búsquedas y reemplazos de texto en C#. Por ejemplo, se pueden utilizar otras clases como `File` y `Stream` para buscar y reemplazar texto en archivos.

También es posible utilizar bibliotecas externas como `OpenTK` y `SharpDX` para realizar búsquedas y reemplazos de texto en juegos y aplicaciones de gráficos.

En resumen, es importante conocer las distintas herramientas disponibles en C# para realizar búsquedas y reemplazos de texto, y elegir la más adecuada para cada situación.

## Ver también

- [Microsoft Docs: Clase String](https://docs.microsoft.com/es-es/dotnet/api/system.string)
- [Regex Class (Microsoft Docs)](https://docs.microsoft.com/es-es/dotnet/api/system.text.regularexpressions.regex)
- [Clase StringBuilder (Microsoft Docs)](https://docs.microsoft.com/es-es/dotnet/api/system.text.stringbuilder)