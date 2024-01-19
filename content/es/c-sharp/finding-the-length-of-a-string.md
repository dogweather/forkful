---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Encuentra la Longitud de un String en C#

## ¿Qué y por qué?

Encontrar la longitud de un string se refiere a determinar el número de caracteres en una secuencia de texto. Los programadores a menudo buscan la longitud para iterar a través de la secuencia o para validar entradas.

## Cómo hacerlo:

En el lenguaje de programación C#, podemos usar la propiedad `Length` del tipo de dato `String` para encontrar su longitud. A continuación, veamos cómo hacerlo.

```C#
string texto = "Hola Mundo!";
int longitud = texto.Length;
Console.WriteLine(longitud);
```

Cuando ejecutes este código, obtendrás la siguiente salida:

```C#
11
```

## Inmersión Profunda

Historicamente, desde el primer lanzamiento de .NET Framework 1.0, el tipo de dato `String` en C# ha tenido la propiedad `Length`. 

Podrías considerar la alternativa de usar el método `Count()` de LINQ, pero es más lento y menos eficiente para esta tarea que `Length`.

```C#
using System.Linq;

string texto = "Hola Mundo!";
int longitud = texto.Count();
Console.WriteLine(longitud);
```

Bajo el capó, la propiedad `Length` devuelve el valor del campo privado `m_stringLength` dentro de la instancia del objeto `String`. Esto es esencialmente una operación O(1), lo cual significa que es muy rápida sin importar la longitud del string.

## Ver También

1. Documentación oficial de la propiedad Length de String en C#: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-6.0)
2. Stack Overflow: [Length vs Count](https://stackoverflow.com/questions/1105990/string-length-vs-string-count)
3. Blog sobre el rendimiento de Length y Count: [DotNetPerls](https://www.dotnetperls.com/string-length)