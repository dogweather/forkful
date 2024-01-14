---
title:                "Gleam: Extrayendo subcadenas"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

# ¿Por qué extraer subcadenas es útil en Gleam?

Extraer subcadenas es una técnica importante en la programación de Gleam que permite manipular cadenas de texto de manera eficiente. Puede ser útil en varias situaciones, como la validación de la entrada del usuario, el procesamiento de datos o la generación de informes.

## Cómo hacerlo

Para extraer una subcadena en Gleam, podemos utilizar la función `String.slice`. Esta función toma dos parámetros: la cadena de origen y el rango (o índices) de la subcadena que queremos extraer. A continuación, se muestra un ejemplo:

```Gleam
let cadena = "¡Hola, mundo!"
let subcadena = String.slice(cadena, 0, 4)
```

En este caso, la variable `subcadena` contendría "¡Hola". Podemos especificar diferentes rangos para obtener diferentes subcadenas. Por ejemplo, si queremos empezar desde el tercer carácter y extraer los siguientes 5 caracteres, podemos utilizar `String.slice(cadena, 2, 6)`.

También podemos utilizar valores negativos para indicar el índice desde el final de la cadena. Por ejemplo, `String.slice(cadena, -5, -1)` extraería "undo" de la cadena original.

## Profundizando en la extracción de subcadenas

La función `String.slice` también puede tomar un tercer parámetro opcional, "step", que indica la cantidad de caracteres a saltar entre cada elemento de la subcadena. Por defecto, el valor de "step" es 1, pero podemos cambiarlo según sea necesario. Por ejemplo, si queremos extraer cada segundo carácter de una cadena, podemos utilizar `String.slice(cadena, 0, 10, 2)`.

Además, podemos utilizar otros métodos para manipular las subcadenas extraídas, como `String.trim` para eliminar espacios vacíos al principio y al final, `String.to_upper_case` para convertir la subcadena a mayúsculas, o `String.reverse` para revertirla.

# Ver también

- Documentación oficial de Gleam sobre las cadenas de texto: https://gleam.run/book/tour/modules/strings.html
- Ejemplos de código de Gleam: https://github.com/gleam-lang/gleam/tree/master/examples
- Tutoriales de Gleam en español: https://medium.com/@jlealtruiz/gleam-programaci%C3%B3n-funcional-en-erlang-98abbd35487