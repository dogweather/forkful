---
title:                "C#: Borrando caracteres que coinciden con un patrón"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, eliminar caracteres que coinciden con un patrón puede ser útil en la programación para limpiar cadenas de texto o realizar operaciones específicas. En esta entrada de blog, exploraremos cómo hacerlo en C#.

## Cómo hacerlo

Para eliminar caracteres coincidentes con un patrón en C#, podemos utilizar la clase `Regex` del espacio de nombres `System.Text.RegularExpressions`. Primero, debemos definir el patrón de caracteres a eliminar utilizando expresiones regulares. Por ejemplo, si queremos eliminar todas las vocales de una cadena de texto, podemos usar el patrón `[aeiouAEIOU]`.

Una vez que tenemos nuestro patrón, podemos crear una instancia de la clase `Regex` y utilizar el método `Replace` para reemplazar los caracteres coincidentes con una cadena vacía. Aquí hay un ejemplo de código:

```C#
string texto = "Hola mundo!";
string patron = "[aeiouAEIOU]";
Regex regex = new Regex(patron);
string resultado = regex.Replace(texto, "");
Console.WriteLine(resultado); // Hl mnd!
```

En este ejemplo, utilizamos el método `Replace` para reemplazar todas las vocales en la cadena `texto` con una cadena vacía, lo que resulta en la cadena `Hl mnd!` como resultado.

También podemos utilizar el método `Match` para obtener un objeto `Match` que represente la primera coincidencia del patrón en una cadena. A partir de ahí, podemos utilizar el método `Remove` para eliminar esa coincidencia. Aquí hay un ejemplo:

```C#
string texto = "Hola mundo!";
string patron = "[aeiouAEIOU]";
Regex regex = new Regex(patron);
Match match = regex.Match(texto);
string resultado = regex.Remove(texto, match.Index);
Console.WriteLine(resultado); // Hla mundo!
```

En este caso, utilizamos el método `Match` para obtener la primera coincidencia del patrón en la cadena `texto`, que es la letra "o". Luego, utilizamos el método `Remove` para eliminar esa coincidencia y obtenemos como resultado la cadena `Hla mundo!`.

## Profundizando

En este ejemplo, solo hemos usado un patrón muy simple para eliminar caracteres de una cadena. Sin embargo, las expresiones regulares ofrecen una gran cantidad de funcionalidades para buscar y reemplazar patrones complejos en texto. Puedes aprender más sobre esto en nuestra guía de expresiones regulares en C# [aquí](https://docs.microsoft.com/es-es/dotnet/standard/base-types/regular-expression-language-quick-reference).

Recuerda que las expresiones regulares pueden ser muy poderosas, pero también pueden ser complicadas de entender. Siempre asegúrate de probar y validar tus patrones antes de utilizarlos en tu código.

## Ver también

- [Guía de expresiones regulares en C#](https://docs.microsoft.com/es-es/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Clase Regex en C#](https://docs.microsoft.com/es-es/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)