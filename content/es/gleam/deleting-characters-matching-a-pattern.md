---
title:                "Gleam: Eliminando caracteres que coinciden con un patrón."
simple_title:         "Eliminando caracteres que coinciden con un patrón."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# ¿Por qué borrar caracteres que coinciden con un patrón en Gleam?

A veces, cuando estamos trabajando con cadenas de texto en Gleam, podemos encontrarnos con la necesidad de eliminar ciertos caracteres que coinciden con un patrón específico. Ya sea por limpieza de datos o por necesidad de un formato específico, esta tarea puede ser una necesidad en ciertas situaciones.

## Cómo hacerlo en Gleam

Hay varias formas de eliminar caracteres que coinciden con un patrón en Gleam, pero una de las maneras más sencillas es utilizando la función `erlang:replace/3`, que nos permite reemplazar una cadena de texto con otra basándonos en un patrón especificado. A continuación, se muestra un ejemplo de cómo utilizar esta función:

```Gleam
import lists.{unzip, zip}

let input = "Gleam es un lenguaje de programación funcional"
let pattern = "l"
let replacement = ""

let output = erlang:replace(input, pattern, replacement)

assert.equal(output, "Geam es unenguaje de programación funciona")
```

En este ejemplo, utilizamos la función `erlang:replace/3` para eliminar todas las letras "l" de la cadena de texto `input`, utilizando una cadena vacía `""` como reemplazo. Como resultado, obtenemos la cadena de texto `output` sin ninguna letra "l".

Otra forma de lograr el mismo resultado sería utilizando pattern matching y recursion. A continuación, se muestra un ejemplo de cómo hacerlo:

```Gleam
import lists.{unzip, zip}

fn remove_pattern(input, pattern) {
  case input {
    [head, ...tail] ->
      remove_pattern(tail, pattern)
    [_, ...tail] ->
      [head | remove_pattern(tail, pattern)]
    [] ->
      []
  }
}

let input = "Gleam es un lenguaje de programación funcional"
let pattern = "l"

let output = remove_pattern(input, pattern)

assert.equal(output, "Geam es unenguaje de programación funciona")
```

En este ejemplo, definimos una función `remove_pattern` utilizando pattern matching y recursion. La función recorre cada caracter de la cadena de texto `input` y va construyendo una nueva cadena de texto `output` sin los caracteres que coinciden con el patrón `pattern`.

En ambos casos, el resultado final sería el mismo. Sin embargo, es importante tener en cuenta que el uso de la función `erlang:replace/3` puede ser más eficiente en términos de rendimiento, especialmente cuando trabajamos con cadenas de texto muy grandes.

## Profundizando en la eliminación de caracteres

Eliminar caracteres que coinciden con un patrón en Gleam es una tarea relativamente sencilla en comparación con otras tareas más complejas que se pueden realizar con cadenas de texto. Sin embargo, es importante tener en cuenta que el manejo de cadenas de texto en Gleam se basa en la biblioteca `erlang:binary`, por lo que es posible utilizar todas las funciones disponibles en esa biblioteca para realizar operaciones más avanzadas como la sustitución de patrones con expresiones regulares o la búsqueda de subcadenas.

En resumen, la eliminación de caracteres que coinciden con un patrón en Gleam es una tarea útil y sencilla que se puede lograr utilizando la función `erlang:replace/3` o mediante el uso de pattern matching y recursion. Además, al comprender el manejo de cadenas de texto en Gleam y la biblioteca `erlang:binary`, podemos llevar a cabo tareas más complejas relacionadas con el procesamiento de cadenas.

# Ver también

- [Documentación oficial de Gleam sobre cadenas de texto](https://gleam.run/documentation/standard-library#text)
- [Documentación oficial de Erlang sobre la biblioteca binary](http://erlang.org/doc/man/binary.html)