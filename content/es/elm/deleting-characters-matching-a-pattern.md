---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elm: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar ciertos caracteres en patrones puede ser útil cuando se trabaja con cadenas de texto, por ejemplo, para limpiar datos o para mejorar la precisión de la búsqueda.

## Cómo hacerlo

Para eliminar caracteres en Elm, se puede utilizar la función `List.filter`. Esta función toma dos argumentos: un predicado que determina si un elemento debe ser eliminado o no, y la lista en la que se realizará la eliminación. Por ejemplo, si queremos eliminar todos los caracteres que sean números en una cadena de texto, podemos hacer lo siguiente:

```elm
stringSinNumeros : String -> String
stringSinNumeros texto =
  texto
    |> String.toList
    |> List.filter (\caracter -> not (Char.isDigit caracter))
    |> String.fromList
```

En este ejemplo, primero convertimos la cadena de texto en una lista con la función `String.toList`, luego usamos `List.filter` con la función anónima `\caracter -> not (Char.isDigit caracter)` como predicado para eliminar todos los caracteres que sean números. Finalmente, convertimos la lista resultante de nuevo en una cadena de texto con la función `String.fromList`.

El siguiente es un ejemplo de entrada y salida utilizando la función `stringSinNumeros`:

```elm
stringSinNumeros "H0l4 m4nd02" --> "Hl mnd"
```

## Deep Dive

La función `List.filter` es muy útil para eliminar elementos de una lista que no cumplan con ciertas condiciones. Como se mencionó anteriormente, toma un predicado como primer argumento, que es una función que toma un elemento y devuelve un `Bool` indicando si ese elemento debe ser eliminado o no. Algunos otros ejemplos de uso de `List.filter` podrían ser:

- Eliminar todos los elementos de una lista que sean nulos (`Nothing`) en una operación de mapeo.
- Eliminar todos los elementos iguales a un valor específico en una lista.
- Eliminar elementos de una lista que no cumplan con ciertas condiciones (mayor a X, menor a Y, etc.).

En resumen, la función `List.filter` es muy versátil y puede utilizarse de diferentes maneras para eliminar elementos de una lista en función de un criterio determinado.

## Ver también

- Documentación oficial de List.filter en Elm: https://package.elm-lang.org/packages/elm/core/latest/List#filter
- Ejemplos de uso de List.filter en Elm: https://guide.elm-lang.org/reuse/removing_from_list.html