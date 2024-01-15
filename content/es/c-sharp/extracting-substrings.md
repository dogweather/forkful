---
title:                "Extrayendo subcadenas"
html_title:           "C#: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has tenido la necesidad de extraer una parte específica de un texto más largo? Ya sea para validar una entrada de usuario, buscar una palabra clave o simplemente manipular datos, la extracción de subcadenas es una herramienta importante para cualquier programador. En este artículo, aprenderemos cómo utilizar esta funcionalidad en el lenguaje de programación C#.

## Cómo hacerlo

Para extraer una subcadena en C#, utilizamos el método `Substring` de la clase `String`. Este método toma dos parámetros: el índice de inicio y la longitud de la subcadena deseada. Por ejemplo, si queremos extraer los primeros 5 caracteres de una cadena, usaríamos el siguiente código:

```C#
string texto = "Hola mundo!";
string subcadena = texto.Substring(0, 5);
```

La variable `subcadena` contendrá la subcadena "Hola". También podemos utilizar un índice negativo para indicar la posición desde el final de la cadena. Por ejemplo, para obtener los últimos 3 caracteres de una cadena, podemos hacer lo siguiente:

```C#
string texto = "Hola mundo!";
string subcadena = texto.Substring(texto.Length - 3, 3);
```

En este caso, la variable `subcadena` contendrá "do!". También podemos omitir el segundo parámetro para extraer la subcadena hasta el final de la cadena, como en el siguiente ejemplo:

```C#
string texto = "Hola mundo!";
string subcadena = texto.Substring(5);
```

En este caso, la variable `subcadena` será "mundo!".

## Profundizando

El método `Substring` hace uso del concepto de índices de cadenas en C#. Un índice es simplemente una posición de un caracter en una cadena, empezando por 0 y contando hacia la derecha. Sin embargo, en C#, también podemos utilizar índices negativos para contar desde el final de la cadena hacia la izquierda.

Además de utilizar el método `Substring`, también podemos acceder a subcadenas utilizando la sintaxis de acceso a índices `[]`. Por ejemplo, si queremos obtener el segundo caracter de una cadena, podemos hacerlo de la siguiente manera:

```C#
string texto = "Hola mundo!";
char caracter = texto[1];
```

En este caso, la variable `caracter` contendrá la letra "o". Esta sintaxis también nos permite acceder a subcadenas, pasando un índice inicial y uno final separados por dos puntos. Por ejemplo, si queremos obtener "mundo" de la cadena "Hola mundo!", podemos hacer lo siguiente:

```C#
string texto = "Hola mundo!";
string subcadena = texto[5..10];
```

También podemos utilizar un índice negativo en esta sintaxis para contar desde el final de la cadena, como en el siguiente ejemplo:

```C#
string texto = "Hola mundo!";
string subcadena = texto[texto.Length - 3..^1];
```

En este caso, la variable `subcadena` contendrá "do!".

¡Ahora eres un experto en la extracción de subcadenas en C#! Puedes utilizar este conocimiento en tus futuros proyectos de programación para manipular y trabajar con cadenas de manera más eficiente.

## Mira también

- [Documentación de Microsoft sobre el método Substring](https://docs.microsoft.com/es-es/dotnet/api/system.string.substring?view=net-5.0)
- [Tutorial de W3Schools sobre el acceso a índices en C#](https://www.w3schools.com/cs/cs_strings_array_indexof.asp)