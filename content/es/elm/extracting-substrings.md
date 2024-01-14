---
title:                "Elm: Extractando subcadenas"
simple_title:         "Extractando subcadenas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por qué extraer subcadenas en Elm es útil

Extraer subcadenas, o porciones de una cadena de texto, es una tarea común en programación. En Elm, esta funcionalidad puede ser muy útil para manipular datos y obtener la información que necesitamos de manera eficiente. A continuación, veremos cómo utilizarla en tus proyectos.

## Cómo hacerlo

Para extraer una subcadena en Elm, utilizamos la función `String.slice start end string`. Esta función recibe tres argumentos: `start`, que es la posición donde queremos comenzar a extraer la subcadena, `end`, que es la posición donde queremos terminar la extracción, y `string`, que es la cadena de texto original.

Por ejemplo, si tenemos la cadena "Hola mundo" y queremos extraer la subcadena "mundo", podemos hacerlo utilizando la función `String.slice 5 9 "Hola mundo"`, ya que la letra "m" está en la posición 5 y la "o" está en la posición 9. El resultado sería "mundo".

También podemos utilizar esta función para obtener un número específico de caracteres a partir de una posición. Por ejemplo, si queremos obtener los tres primeros caracteres de la cadena "Elm es divertido", podemos utilizar `String.slice 0 3 "Elm es divertido"` y el resultado sería "Elm".

Si queremos extraer una subcadena a partir del final de la cadena original, podemos utilizar números negativos en `start` y `end`. Por ejemplo, si queremos extraer la subcadena "divertido" de la cadena "Elm es divertido", podemos hacerlo con `String.slice -8 -1 "Elm es divertido"`, ya que la "d" está en la posición -8 y la "o" está en la posición -1.

## Profundizando

Además de la función `String.slice`, Elm ofrece otras funciones para trabajar con subcadenas. Por ejemplo, `String.left n string` nos permite obtener los primeros `n` caracteres de una cadena, mientras que `String.right n string` nos devuelve los últimos `n` caracteres.

También podemos utilizar `String.drop n string` para eliminar los primeros `n` caracteres de una cadena y `String.dropRight n string` para eliminar los últimos `n` caracteres.

Otras funciones útiles incluyen `String.take n string`, que nos permite obtener los primeros `n` caracteres de una cadena y descartar el resto, y `String.takeRight n string`, que hace lo mismo pero con los últimos `n` caracteres.

## Ver también

- Documentación oficial de Elm sobre `String.slice`: https://package.elm-lang.org/packages/elm/core/latest/String#slice
- Tutorial de Elm para principiantes: https://guide.elm-lang.org/
- Ejemplos de código en Elm: https://github.com/elm/projects/1