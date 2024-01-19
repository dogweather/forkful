---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Las expresiones regulares, o regex, son una forma de encontrar patrones en las cadenas de texto. Los programadores las usan para operaciones como buscar, reemplazar y validar cadenas.

## Cómo se hace:

Utilicemos un ejemplo sencillo para ver cómo se implementan las expresiones regulares en C#. Imagine que necesita verificar si un texto contiene un número de teléfono.

```C#
using System.Text.RegularExpressions;

string pattern = @"(\d{3})-\d{3}-\d{4}";
string input = "Llámame al 555-123-4567 por favor.";

if(Regex.IsMatch(input, pattern))
{
  Console.WriteLine("Un número de teléfono es detectado.");
} 
else 
{
  Console.WriteLine("No se detecta ningún número telefónico.");
}
```

La salida será: "Un número de teléfono es detectado."

## Análisis a Profundidad:

Las expresiones regulares tienen una amplia historia y se han implementado en varios lenguajes de programación, incluyendo C#. Ofrecen una forma poderosa y flexible de manejar texto, pero también pueden ser bastante complejas y difíciles de leer si se mal utilizan.

Existen alternativas a las regex para manejar cadenas de texto, dependiendo del caso de uso. Existe la opción de usar métodos incorporados en el lenguaje, como los de la clase `string` en C# para manipulación de cadenas. 

Implemetar regex en C# es bastante sencillo gracias a la clase `Regex`. Esta clase incluye varios métodos útiles como `IsMatch`, `Match`, `Matches`, `Replace` y otros que facilitan su manejo.

## Ver También:

Para aprender más acerca de las expresiones regulares, recomiendo los siguientes enlaces (todos en inglés):

1. [La documentación oficial de Microsoft](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
2. [Un tutorial en W3Schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
3. [Un creador y tester de expresiones regulares](https://regexr.com/)
4. [La guía de StackOverflow sobre expresiones regulares](https://stackoverflow.com/questions/22937618/reference-what-does-this-regex-mean)