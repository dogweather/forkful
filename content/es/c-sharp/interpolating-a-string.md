---
title:                "Interpolando una cadena"
html_title:           "C#: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué hacerlo?

Interpolar una cadena en programación se refiere a la capacidad de insertar valores de variables dentro de una cadena de texto. Los programadores lo hacen para hacer que sus cadenas de texto sean más dinámicas y personalizadas, permitiéndoles mostrar información específica en diferentes situaciones.

## Cómo hacerlo:

```c#
string nombre = "Juan";
int edad = 25;

// Interpolando una cadena con valores de variables
string mensaje = $"¡Hola! Mi nombre es {nombre} y tengo {edad} años.";
Console.WriteLine(mensaje);

/* Output
¡Hola! Mi nombre es Juan y tengo 25 años. 
*/

// También se pueden interpolar valores directamente
string mensaje2 = $"El resultado de 5 + 5 es {5 + 5}.";
Console.WriteLine(mensaje2);

/* Output
El resultado de 5 + 5 es 10.
*/
```

## Profundizando:

Interpolar cadenas en C# fue introducido en la versión 6 del lenguaje en el año 2015. Antes de eso, se usaba la concatenación de cadenas mediante el operador `+` o el método `String.Format()`. Sin embargo, la interpolación de cadenas es más legible y fácil de usar. También es posible utilizar valores de propiedades y métodos dentro de la interpolación de cadenas.

## Ver también:

- [Documentación oficial de C# sobre interpolación de cadenas](https://docs.microsoft.com/es-es/dotnet/csharp/language-reference/tokens/interpolated)
- [Artículo sobre las diferencias entre interpolación de cadenas y concatenación en C#](https://coderwall.com/p/bk9npq/c-interpolacion-de-cadenas-vs-concatenacion-de-cadenas)