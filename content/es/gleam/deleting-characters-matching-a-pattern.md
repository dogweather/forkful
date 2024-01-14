---
title:                "Gleam: Eliminando caracteres que coinciden con un patrón"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón puede ser una tarea útil en la programación para limpiar y organizar datos, o para crear una lógica más específica en el código.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Gleam, utilizaremos la función `delete_chars` combinada con una expresión regular. Un ejemplo de código sería el siguiente:

```Gleam
import gleam/regex

// creamos una expresión regular para encontrar caracteres que coinciden con nuestro patrón
let regex = regex.regex("\d")

// eliminamos los caracteres que coinciden con el patrón en la cadena "Hola 123"
let result = regex.delete_chars("Hola 123")

// imprimimos el resultado
debug!("%s", result) // salida: "Hola "
```

Podemos ver que todos los caracteres que coinciden con el patrón `\d`, que representa cualquier número, han sido eliminados de la cadena.

## Profundizando

Al utilizar una expresión regular en combinación con la función `delete_chars`, podemos ser más específicos en cuanto a qué caracteres queremos eliminar. Esto nos permite tener un control más preciso sobre cómo limpiamos y organizamos nuestros datos.

Además, también podemos utilizar la función `delete_chars` en conjunto con la función `map` para eliminar caracteres que coinciden con un patrón en una lista de cadenas, en lugar de en una sola cadena.

```Gleam
import gleam/regex

// creamos una expresión regular para encontrar caracteres que coinciden con nuestro patrón
let regex = regex.regex("[:punct:]")

// eliminamos los caracteres que coinciden con el patrón en la lista de cadenas
let results = ["Hola!", "¿Cómo estás?", "Bien, gracias"] |> List.map(regex.delete_chars)

// imprimimos el resultado
debug!("%s", results) // salida: ["Hola", "¿Cómo estás", "Bien gracias"]
```

En este ejemplo, utilizamos la clase de caracteres `punct` en nuestra expresión regular para eliminar todos los signos de puntuación de las cadenas de la lista.

## Ver también

Para más información sobre cómo utilizar la función `delete_chars` en Gleam, puede consultar la documentación oficial en su página web [aquí](https://gleam.run/modules/regex/), o el repositorio de Gleam en Github [aquí](https://github.com/gleam-lang/gleam). ¡Feliz limpieza de datos!