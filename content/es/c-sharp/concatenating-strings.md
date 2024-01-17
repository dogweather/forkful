---
title:                "Concatenando cadenas"
html_title:           "C#: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

¿Qué es la concatenación de cadenas y por qué los programadores lo hacen?

La concatenación de cadenas es la acción de unir varias cadenas de texto en una sola cadena más larga. Los programadores utilizan esta técnica para crear cadenas personalizadas para imprimir o mostrar en pantalla, o para almacenar como una variable en su programa.

Cómo hacerlo:

Hay varios métodos para concatenar cadenas en C#, pero uno de los más comunes es utilizando el operador "+" para unir dos o más cadenas. Por ejemplo:

```C#
string frase1 = "¡Hola, ";
string frase2 = "amigos!";
string fraseFinal = frase1 + frase2;
Console.WriteLine(fraseFinal);
```

La salida de este código sería: "¡Hola, amigos!".

Otra forma de concatenar cadenas es utilizando el método `String.Concat()` que permite unir más de dos cadenas. Por ejemplo:

```C#
string nombre = "Juan";
string apellido = "García";
string nombreCompleto = String.Concat(nombre, " ", apellido);
Console.WriteLine(nombreCompleto);
```

La salida sería: "Juan García".

Profundizando:

La concatenación de cadenas no es algo nuevo en la programación, ya que ha existido desde los primeros lenguajes de programación. Sin embargo, la forma en que se realiza puede variar de un lenguaje a otro. En C#, además de los métodos mencionados anteriormente, también se puede utilizar el método `String.Format()` para concatenar y formatear cadenas al mismo tiempo.

Existen otras alternativas a la concatenación de cadenas, como por ejemplo utilizar las clases `StringBuilder` o `StringJoiner` que se utilizan especialmente para la creación de cadenas complejas.

En cuanto a la implementación, la concatenación de cadenas en C# no es tan sencilla como podría parecer. Al unir dos cadenas, se crea en realidad una nueva cadena y se elimina la cadena original, lo que puede afectar el rendimiento en casos de manipulación de grandes cantidades de cadenas.

Te puede interesar:

- [Documentación oficial de Microsoft sobre concatenación de cadenas en C#](https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/strings/)
- [Métodos para la manipulación de cadenas en C#](https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/strings/index)
- [Ejemplos de código de concatenación de cadenas en C#](https://www.edureka.co/blog/concatenate-string-c-sharp/)