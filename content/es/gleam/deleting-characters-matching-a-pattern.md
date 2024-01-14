---
title:    "Gleam: Eliminando caracteres que coincidan con un patrón"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón puede ser útil en diferentes situaciones, como por ejemplo, limpiar datos o filtrar información específica. En este artículo, aprenderás cómo realizar esta tarea utilizando el lenguaje de programación Gleam.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Gleam, podemos utilizar la función built-in `String.replace` y especificar el patrón a eliminar. Por ejemplo, si queremos eliminar todos los caracteres "a" de una cadena, podemos utilizar el siguiente código:

```Gleam
let cadena = "Hola amigo"
let nueva_cadena = String.replace(cadena, "a", "")
```

La nueva cadena resultante será "Hol migo", ya que se han eliminado todas las apariciones de "a". Además de especificar un carácter, también podemos utilizar expresiones regulares para eliminar patrones más complejos. Por ejemplo, para eliminar todos los números de una cadena podemos utilizar el siguiente código:

```Gleam
let cadena = "Hoy es 12 de julio"
let nueva_cadena = String.replace(cadena, "\\d+", "")
```

En este caso, la nueva cadena resultante será "Hoy es de julio", ya que se han eliminado todos los números del patrón especificado en la expresión regular.

## Deep Dive

Si queremos un mayor control sobre cómo se eliminan los caracteres que coinciden con un patrón, podemos utilizar la función `String.replacer` en lugar de `String.replace`. Esta función nos permite especificar una función de reemplazo personalizada que determina qué caracteres serán reemplazados y por qué.

Por ejemplo, si queremos eliminar todos los espacios en blanco de una cadena pero dejar un espacio entre cada palabra, podemos utilizar la siguiente función de reemplazo personalizada:

```Gleam
fn(replacement) {
  match replacement {
    " " => " ",
    _ => ""
  }
}
```

Al utilizar esta función con la función `String.replacer`, podemos eliminar todos los espacios en blanco excepto el primero entre palabras:

```Gleam
let cadena = "Hola      amigo"
let nueva_cadena = String.replacer(cadena, fn)
```

La nueva cadena resultante será "Hola amigo", con un solo espacio entre las palabras.

## Ver también

- Documentación de Gleam sobre `String.replace`: https://gleam.run/modules/string#replace
- Tutorial de expresiones regulares en Gleam: https://davembush.github.io/regular-expressions-in-gleam/
- Ejemplos de uso de `String.replace` en Gleam: https://gleam-examples.github.io/string/#replace