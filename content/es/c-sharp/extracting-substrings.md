---
title:                "C#: Extrayendo subcadenas"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué
Extracción de subcadena es una técnica útil cuando necesitas obtener un fragmento específico de una cadena de texto más grande. Esto puede ser útil en una variedad de escenarios, como procesamiento de texto, análisis de datos y manipulación de cadenas de caracteres.

## Cómo Hacerlo
Para extraer una subcadena de una cadena de texto en C#, podemos utilizar el método `Substring()` de la clase `String`. Este método toma dos parámetros: el índice de inicio y la longitud de la subcadena que deseas extraer. Veamos un ejemplo en código:

```C#
string fullName = "Ana Rodríguez";
string lastName = fullName.Substring(4, 9);
Console.WriteLine(lastName);
```
Salida: Rodríguez

En este ejemplo, estamos extrayendo la subcadena "Rodríguez" de la cadena completa "Ana Rodríguez" utilizando el índice de inicio 4 y una longitud de 9 caracteres.

También es posible utilizar el método `Split()` de la clase `String` para extraer subcadenas de una cadena de texto. Este método divide la cadena en una matriz de subcadenas utilizando un separador específico. Aquí hay un ejemplo:

```C#
string sentence = "Hola, bienvenidos a mi blog";
string[] words = sentence.Split(',');
foreach (string word in words)
{
    Console.WriteLine(word);
}
```
Salida: Hola
Bienvenidos a mi blog

En este ejemplo, estamos extrayendo dos subcadenas de la cadena "Hola, bienvenidos a mi blog" utilizando la coma como separador.

## Profundizando
Existen varios métodos y técnicas más avanzadas para extraer subcadenas en C#. Por ejemplo, también podemos utilizar expresiones regulares para manipular y extraer subcadenas de cadenas de texto. Además, hay varios métodos de lenguaje integrados en C# que pueden ser útiles, como `IndexOf()` y `LastIndexOf()`. Es importante explorar y experimentar con estas técnicas para encontrar la mejor forma de extraer subcadenas en diferentes escenarios.

## Ver también
- [Método Substring en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.string.substring)
- [Método Split en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.string.split)
- [Expresiones regulares en C#](https://www.c-sharpcorner.com/UploadFile/8911c4/regular-expression-in-C-Sharp/)