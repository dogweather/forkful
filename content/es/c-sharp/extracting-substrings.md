---
title:                "Extracción de subcadenas"
html_title:           "C#: Extracción de subcadenas"
simple_title:         "Extracción de subcadenas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La extracción de subcadenas es una técnica que permite obtener una porción de una cadena de texto más grande. Los programadores la utilizan para manipular y gestionar datos de manera más eficiente.

## Cómo:
```C#
// Ejemplo 1: Usando el método Substring()
string cadena = "Hola mundo";
string subcadena = cadena.Substring(5);
// subcadena = "mundo"

// Ejemplo 2: Usando el operador de acceso a índices []
string cadena = "Hola mundo";
string subcadena = cadena[0..4];
// subcadena – "Hola"

// Ejemplo 3: Usando la clase StringReader y el método Read()
string cadena = "Hola mundo";
StringReader reader = new StringReader(cadena);
char[] buffer = new char[5];
reader.Read(buffer, 0, 5);
// buffer = "Hola "
```

## Profundizando:
La extracción de subcadenas es una técnica comúnmente utilizada en programación desde los inicios de la informática. También se puede lograr mediante el uso de métodos como Split() o Regex.Match(). En algunos lenguajes de programación, como Python, las cadenas de texto son inmutables, lo que significa que no se pueden modificar una vez creadas. Sin embargo, en C# las cadenas son mutables, lo que permite una mayor flexibilidad en la manipulación de datos.

## Véase también:
- [Documentación oficial de Microsoft sobre el método Substring()](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
- [Tutorial para aprender a utilizar expresiones regulares en C#](https://www.c-sharpcorner.com/article/regexparser-learn-regular-expressions-in-c-sharp/)
- [Artículo sobre las diferencias entre cadenas de texto inmutables y mutables en Python](https://code.tutsplus.com/es/tutorials/understanding-immutable-strings-in-python--cms-26168)