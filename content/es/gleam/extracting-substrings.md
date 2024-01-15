---
title:                "Extrayendo subcadenas"
html_title:           "Gleam: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Si alguna vez has trabajado con cadenas de texto en tus proyectos de programación, es probable que te hayas encontrado en la necesidad de extraer una parte específica de una cadena. Ya sea para obtener información relevante o para manipularla de alguna manera, extraer subcadenas es una habilidad valiosa en el mundo de la programación. En este artículo, te mostraremos cómo puedes extraer subcadenas utilizando Gleam, un lenguaje de programación elegante y eficiente.

## Cómo hacerlo

La función `String.substring` en Gleam te permitirá extraer una subcadena de una cadena dada. A continuación te mostramos un ejemplo de cómo usar esta función en tu código:

```Gleam
let cadena = "Este es un ejemplo"
let subcadena = String.substring(cadena, 5, 7)
```

En este caso, la subcadena que se extraerá será "es". El primer parámetro de la función es la cadena original, el segundo es la posición inicial y el tercero es la longitud de la subcadena que deseas obtener. Ten en cuenta que la posición inicial comienza en 0.

Otra función útil que puede ayudarte a obtener subcadenas es `String.take`. Esta función toma como argumento una cadena y un número determinado de caracteres y devuelve los primeros caracteres de la cadena. Por ejemplo:

```Gleam
let cadena = "Hola mundo"
let subcadena = String.take(cadena, 4)
```

En este caso, la subcadena sería "Hola".

## Inmersión Profunda

Es importante tener en cuenta que los índices utilizados para extraer subcadenas en Gleam no son necesariamente los mismos que en otros lenguajes de programación. En Gleam, los índices se calculan utilizando los puntos de código Unicode, lo que permite que los caracteres no ASCII se manejen correctamente.

Además, puedes utilizar la función `String.codepoint` para obtener el punto de código Unicode de un determinado carácter en una cadena. Esto puede ser útil si necesitas extraer una subcadena basada en puntos de código en lugar de posiciones de caracteres.

Finalmente, existen otras funciones en la biblioteca estándar de Gleam que pueden ayudarte a manipular y extraer subcadenas. Te recomendamos que explores la documentación y pruebes diferentes opciones para encontrar la mejor solución para tus necesidades específicas.

## Ver También
- Documentación oficial de Gleam sobre extracción de subcadenas: https://gleam.run/documentation/stdlib/string.html#substring
- Ejemplos de uso de la función `String.substring`: https://gleam.run/documentation/examples/substring.html
- Tutorial sobre manipulación de cadenas en Gleam: https://gleam.run/tutorials/strings.html