---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "C#: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué?

A veces, en programación, necesitamos realizar acciones específicas en una cadena de texto. Esto puede incluir eliminar caracteres que siguen un patrón determinado. En este artículo, te mostraremos cómo hacer esto de manera sencilla utilizando C#.

## Cómo

Para eliminar caracteres que coinciden con un patrón en C#, podemos utilizar el método `Regex.Replace()` de la librería `System.Text.RegularExpressions`. Este método toma tres parámetros: la cadena original, el patrón que queremos eliminar y la cadena de reemplazo.

Por ejemplo, si queremos eliminar todas las vocales de una cadena, el patrón sería `[aeiou]` y la cadena de reemplazo sería una cadena vacía. El código se vería así:

```C#
string original = "Hola mundo!";
string patron = "[aeiou]";
string reemplazo = "";

string resultado = Regex.Replace(original, patron, reemplazo);

Console.WriteLine(resultado); // Hl mnd!
```

En este ejemplo, el método `Regex.Replace()` buscará en la cadena original las vocales (a, e, i, o, u) y las remplazará con una cadena vacía, logrando eliminarlas.

Otra forma de eliminar caracteres es utilizando el método `Trim()` que sirve para eliminar caracteres al principio y al final de una cadena. Por ejemplo, si queremos eliminar los espacios en blanco al principio y al final de una cadena, podemos usar este método de la siguiente forma:

```C#
string original = "    Hola mundo!    ";
string resultado = original.Trim();

Console.WriteLine(resultado); // Hola mundo!
```

## Deep Dive

En nuestro primer ejemplo, el patrón utilizado era una expresión regular. Las expresiones regulares son secuencias de caracteres que nos permiten buscar patrones en una cadena. Pueden ser muy útiles para realizar búsquedas y reemplazos más complejos.

Por otro lado, el método `Trim()` también puede tomar un parámetro opcional que indica qué caracteres queremos eliminar en lugar de solo los espacios en blanco. Por ejemplo, si queremos eliminar todos los signos de puntuación al principio y al final de una cadena, podemos utilizar el siguiente código:

```C#
string original = "¡Hola mundo!";
char[] caracterAEliminar = { '¡', '!' };
string resultado = original.Trim(caracterAEliminar);

Console.WriteLine(resultado); // Hola mundo
```

## Ver También

- [Documentación oficial de C#](https://docs.microsoft.com/es-es/dotnet/csharp/)
- [Explicación detallada de expresiones regulares en C#](https://docs.microsoft.com/es-es/dotnet/standard/base-types/regular-expression-language-quick-reference)