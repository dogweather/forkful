---
title:                "C#: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Existe una función en C# conocida como "Substring", la cual permite extraer partes de una cadena de texto. Esto puede ser útil en muchas situaciones, como por ejemplo cuando se desea manipular solo una parte de un texto, o cuando se necesita realizar comparaciones entre distintas secciones de una cadena. En este post, exploraremos cómo podemos aprovechar esta función para extraer subcadenas de manera eficiente.

## Cómo

La función "Substring" se utiliza para extraer una subcadena de una cadena más grande. Para ello, se le debe proporcionar dos parámetros: el índice de inicio y la longitud de la subcadena deseada.

```C#
string texto = "Este es un ejemplo de cadena de texto";
string subcadena = texto.Substring(11, 7);
Console.WriteLine(subcadena);
```

Este código imprimirá la subcadena "ejemplo", ya que estamos indicando que se inicie en el índice 11 y se extraigan 7 caracteres. Tenga en cuenta que los índices en C# comienzan desde 0, por lo que el primer carácter de la cadena tendría un índice de 0.

Además, la función también puede ser utilizada para extraer una subcadena desde un índice específico hasta el final de la cadena.

```C#
string texto = "Este es un ejemplo de cadena de texto";
string subcadena = texto.Substring(20);
Console.WriteLine(subcadena);
```

Este código imprimirá la subcadena "cadena de texto", ya que estamos indicando que se extraiga desde el índice 20 hasta el final de la cadena.

## Profundizando

Es importante recordar que la función "Substring" no modifica la cadena original, sino que devuelve una nueva cadena como resultado. Por lo tanto, si se realizan cambios en la subcadena, no afectarán a la cadena original.

Además, también se debe tener en cuenta que si se proporciona un índice o una longitud que esté fuera de los límites de la cadena, se producirá un error. Por ejemplo, si intentamos extraer una subcadena a partir del índice 30 en la cadena "Hola", se producirá un error ya que la cadena solo tiene 4 caracteres.

Teniendo en cuenta estas consideraciones, podemos aprovechar al máximo la función "Substring" en nuestras aplicaciones, ya sea para manipular datos o para realizar comparaciones.

## Ver también

- [Microsoft Docs - String.Substring Method](https://docs.microsoft.com/es-es/dotnet/api/system.string.substring)
- [Tutorial de C# - Cadenas](https://www.tutorialsteacher.com/csharp/csharp-string)