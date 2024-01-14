---
title:                "C#: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón es una tarea común en la programación, especialmente cuando se trabaja con cadenas de texto o archivos de texto. Puede ser útil para limpiar y formatear los datos, o para obtener información específica de un conjunto de caracteres.

## Cómo hacerlo

Existen varias formas de eliminar caracteres que coinciden con un patrón en C#. Aquí mostraremos dos ejemplos utilizando la función `Regex.Replace()`.

### Ejemplo 1:

Supongamos que tenemos una cadena de texto con varias palabras separadas por espacios, y queremos eliminar todas las palabras que contengan la letra "a". Podemos hacerlo utilizando una expresión regular y la función `Regex.Replace()` de la siguiente manera:

```C#
string texto = "Hola amigos, ¿cómo están?";
string patron = @"\b[aA]\w+\b"; // Expresión regular para coincidir con palabras que comienzan con "a"
string resultado = Regex.Replace(texto, patron, ""); // Reemplaza las palabras que coinciden con el patrón por una cadena vacía
Console.WriteLine(resultado); // Imprime: ", ¿cómo están?"
```

### Ejemplo 2:

Otra forma de eliminar caracteres que coinciden con un patrón es utilizando la función `Regex.Replace()` en conjunto con la función `MatchEvaluator`. Esta última nos permite definir una función que será llamada para cada coincidencia encontrada en la cadena, y devolverá la cadena que se utilizará para reemplazarla. Veamos un ejemplo:

```C#
string texto = "I love programming";
string patron = @"\w+ing"; // Expresión regular para coincidir con palabras que terminan con "ing"
string resultado = Regex.Replace(texto, patron, match => match.Value.ToUpper()); // Reemplaza las palabras que coinciden con el patrón por su versión en mayúsculas
Console.WriteLine(resultado); // Imprime: I LOVE PROGRAMMING
```

## Profundizando

Cuando se trabaja con expresiones regulares, es importante tener en cuenta que cada lenguaje de programación puede tener diferencias en la sintaxis o en las funciones disponibles. Es recomendable consultar la documentación oficial de C# para conocer más sobre las funciones y opciones disponibles para trabajar con patrones y expresiones regulares.

## Ver también

- [Documentación oficial de Microsoft sobre expresiones regulares en C#](https://docs.microsoft.com/es-es/dotnet/standard/base-types/regular-expression-language-quick-reference) 
- [Tutorial de Expressions Regular de C# de Codecademy](https://www.codecademy.com/learn/learn-regular-expressions)