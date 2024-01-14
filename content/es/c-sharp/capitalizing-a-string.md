---
title:    "C#: Capitalizar una cadena"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena de texto es una tarea común en la programación, especialmente cuando se trabaja con nombres o títulos de texto. Al capitalizar una cadena, se asegura de que la primera letra de cada palabra esté en mayúscula, lo que facilita la lectura y presentación de la información.

## Cómo hacerlo

Para capitalizar una cadena en C#, podemos usar el método `ToUpper` de la clase `TextInfo`. Primero, necesitamos importar el espacio de nombres `System.Globalization` en nuestro código. Luego, creamos una instancia de la clase `TextInfo` y usamos el método `ToUpper` para capitalizar la cadena deseada.

```
using System.Globalization;

string nombre = "maría";
TextInfo ti = CultureInfo.CurrentCulture.TextInfo;

string nombreCapitalizado = ti.ToUpper(nombre);
Console.WriteLine(nombreCapitalizado);

// Output: María
```

También podemos utilizar el método `ToTitleCase` de la clase `TextInfo` para capitalizar solo la primera letra de cada palabra en la cadena.

```
using System.Globalization;

string titulo = "el gran gatsby";
TextInfo ti = CultureInfo.CurrentCulture.TextInfo;

string tituloCapitalizado = ti.ToTitleCase(titulo);
Console.WriteLine(tituloCapitalizado);

// Output: El Gran Gatsby
```

## Profundizando

Existen algunas consideraciones importantes al capitalizar una cadena en C#. Por ejemplo, si la cadena contiene caracteres unicode, el método `ToUpper` no los capitalizará correctamente. En cambio, podemos usar el método `ToUpperInvariant` que trata los caracteres unicode de manera adecuada.

Otra cosa a tener en cuenta es que si estamos trabajando con una cadena en mayúsculas, el método `ToUpper` no cambiará nada en la cadena original. En su lugar, podemos usar el método `ToLower` para forzar la capitalización.

```
using System.Globalization;

string titulo = "EL GRAN GATSBY";
TextInfo ti = CultureInfo.CurrentCulture.TextInfo;

string tituloCapitalizado = ti.ToLower(titulo);
Console.WriteLine(tituloCapitalizado);

// Output: El gran gatsby
```

¡Con estos métodos, capitalizar una cadena en C# es fácil y nos aseguramos de tener una salida correcta en cualquier situación!

## Ver también

- [String.ToUpper Method (System) - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netcore-3.1)
- [Examples of String Capitalization in C# - C# Corner](https://www.c-sharpcorner.com/article/string-capitalization-in-C-Sharp/)