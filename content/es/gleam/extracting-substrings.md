---
title:                "Gleam: Extractando subcadenas"
simple_title:         "Extractando subcadenas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas de texto es una tarea común en la programación, ya sea para manipular datos o para mostrar contenido dinámicamente. En este artículo, aprenderemos cómo hacerlo en Gleam y profundizaremos en los detalles para que puedas utilizarlo en tus propios proyectos.

## Cómo hacerlo

El método para extraer subcadenas en Gleam es muy sencillo. Simplemente utilizamos la función `String.slice` y le pasamos el índice inicial y final del texto que queremos extraer, separados por dos puntos.

```Gleam
extraction = String.slice("Hola mundo!", 0:4)
```

El ejemplo anterior extraerá los primeros cuatro caracteres de la cadena, en este caso "Hola". Si queremos extraer desde un índice hasta el final, solo omitimos el segundo número.

```Gleam
rest = String.slice("Hola mundo!", 5:)
```

En este caso, la variable `rest` contendrá la subcadena "mundo!". También podemos hacer uso de números negativos para empezar a contar desde el final de la cadena.

```Gleam
last_word = String.slice("Hola mundo!", -5:)
```

En este ejemplo, `last_word` será igual a "mundo!".

## Profundizando

Si deseamos extraer una subcadena basándonos en un patrón específico, podemos utilizar la función `String.split` y proporcionar el patrón como argumento.

```Gleam
parts = String.split("Hola, cómo estás?", ", ")
```

En este caso, `parts` contendrá una lista con dos elementos, "Hola" y "cómo estás?". También podemos proporcionar un patrón de expresión regular para una extracción más precisa.

## Ver también

- Function `String.slice`: https://gleam.run/core/string.html#slice
- Function `String.split`: https://gleam.run/core/string.html#split
- Expressions regulares: https://gleam.run/core/regexp.html