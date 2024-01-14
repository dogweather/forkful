---
title:                "Elm: Extrayendo subcadenas"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Elm?

Extraer subcadenas es una tarea común en la programación, especialmente cuando se trabaja con texto. En Elm, esta funcionalidad se puede lograr de forma fácil y eficiente utilizando algunos de sus métodos predefinidos. En este artículo, exploramos por qué es útil extraer subcadenas en Elm y cómo hacerlo de manera efectiva.

## Cómo extraer subcadenas en Elm

Extraer subcadenas en Elm es un proceso sencillo que se puede realizar utilizando la función `String.slice` o el operador `String.left`. Ambos métodos aceptan dos argumentos: el índice de inicio y el índice de finalización de la subcadena que se desea extraer. Por ejemplo, si queremos extraer la subcadena "Elm" de la cadena "¡Hola Elm!", podemos usar la función `String.slice` de la siguiente manera:

```Elm
String.slice 6 8 "¡Hola Elm!" -- devuelve "Elm"
```

O podemos usar el operador `String.left`, que es una forma más abreviada de lograr lo mismo:

```Elm
"¡Hola Elm!" |> String.left 3 -- devuelve "Elm"
```

Ambos métodos también pueden aceptar argumentos negativos, lo que nos permite comenzar a extraer desde el final de la cadena hacia atrás. Por ejemplo, si queremos extraer la subcadena "Elm" de la cadena "¡Hola Elm!" pero comenzando desde el final, podemos hacerlo así:

```Elm
String.slice -3 -1 "¡Hola Elm!" -- devuelve "Elm"
```

Estos métodos también pueden ser útiles si queremos extraer una parte específica de una URL o de un nombre de archivo. Por ejemplo, si queremos extraer el nombre del archivo de una URL, podemos usar el operador `String.left` de la siguiente manera:

```Elm
"https://miweb.com/ejemplo.html" |> String.left 14 -- devuelve "ejemplo.html"
```

## Profundizando en la extracción de subcadenas

Más allá de la función `String.slice` y el operador `String.left`, Elm también tiene otros métodos que podemos utilizar para extraer subcadenas de manera más específica. Por ejemplo, la función `String.leftMap` nos permite aplicar una función a cada carácter de la subcadena extraída. Esto puede ser útil para realizar operaciones en cada carácter de una palabra o para transformar una subcadena en una lista de caracteres. También existe la función `String.leftWhile` que nos permite extraer la subcadena hasta que se cumpla una determinada condición.

Si necesitamos extraer una subcadena basada en un patrón, podemos utilizar la función `String.split` que nos permite dividir una cadena en una lista de subcadenas basadas en un carácter o conjunto de caracteres. Incluso podemos usar la función `String.contains` para verificar si una subcadena específica se encuentra dentro de otra cadena.

## Ver también

- [Documentación oficial de Elm sobre la extracción de subcadenas](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- [Artículo de blog sobre la manipulación de cadenas en Elm](https://yalishizhude.github.io/2017/05/29/string-in-elm.html)
- [Repositorio de ejemplos de Elm en GitHub](https://github.com/elm/example)

¡Ahora ya sabes cómo extraer subcadenas en Elm y puedes incorporar esta funcionalidad en tus proyectos de manera eficiente! ¡Sigue explorando y aprovechando al máximo el lenguaje de programación funcional más amigable!