---
title:                "Elm: Borrando caracteres que coinciden con un patrón"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# ¿Por qué borrar caracteres coincidentes con un patrón en Elm?

En programación, a menudo nos encontramos con la necesidad de manipular texto de alguna forma. Esto puede ser para formatearlo, filtrarlo o eliminar ciertos caracteres. Una forma útil de hacer esto en Elm es mediante la eliminación de caracteres que coinciden con un patrón específico. En esta publicación de blog, aprenderemos por qué y cómo hacerlo en Elm.

## ¿Cómo hacerlo?

Para eliminar caracteres que coinciden con un patrón en Elm, podemos usar la función `String.delete` de la biblioteca `elm/string`. Esta función toma dos argumentos: el patrón que queremos eliminar y el string en el que queremos buscar. Por ejemplo, si queremos eliminar todas las letras "a" de un string, podemos hacerlo de la siguiente manera:

```Elm
import String

String.delete "a" "hola" -- produce "hol"
```

También podemos usar patrones más complejos, como expresiones regulares, utilizando la función `Regex.replace` de la biblioteca `elm/regex`. Esta función toma tres argumentos: el patrón que queremos reemplazar, el nuevo string que queremos insertar y el string en el que queremos buscar. Por ejemplo, si queremos reemplazar todos los números en un string con la palabra "número", podemos hacerlo de la siguiente manera:

```Elm
import Regex

Regex.replace (Regex.regex "\\d") "número" "123 hola" -- produce "número número número hola"
```

## Profundizando

La función `String.delete` es útil para eliminar caracteres específicos de un string, pero a veces queremos ser más específicos sobre qué caracteres queremos eliminar. Para ello, podemos usar la función `String.filter` de la misma biblioteca. Esta función toma un predicado como argumento y elimina todos los caracteres del string que no cumplan con ese predicado. Por ejemplo, si solo queremos eliminar las letras minúsculas de un string, podemos hacerlo de la siguiente manera:

```Elm
import String

onlyUpper : Char -> Bool
onlyUpper char =
    Char.isUpper char -- devuelve true si el carácter es una letra mayúscula

String.filter onlyUpper "HeLLo" -- produce "HL"
```

¡También puedes combinar `String.filter` con `String.delete` para eliminar caracteres que cumplan cierta condición y luego eliminar específicamente otros caracteres!

## Ver también

- [Documentación de `elm/string`](https://package.elm-lang.org/packages/elm/string/latest/)
- [Documentación de `elm/regex`](https://package.elm-lang.org/packages/elm/regex/latest/)