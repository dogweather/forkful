---
title:                "Borrando caracteres que coincidan con un patrón"
html_title:           "Gleam: Borrando caracteres que coincidan con un patrón"
simple_title:         "Borrando caracteres que coincidan con un patrón"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

¡Hola programadores de Gleam!

¿Alguna vez te has encontrado con la necesidad de eliminar ciertos caracteres de una cadena de texto que coinciden con un patrón específico? En Gleam, esto se conoce como "borrar caracteres que coinciden con un patrón". ¿Y por qué los programadores hacen esto? Bueno, a veces necesitamos limpiar una cadena de entrada antes de analizarla o simplemente queremos reemplazar caracteres no deseados.

## ¿Qué y por qué?

"Borrar caracteres que coinciden con un patrón" se refiere a eliminar ciertos caracteres de una cadena de texto que encajan con un patrón establecido. Esto es especialmente útil cuando queremos manipular una cadena de texto de manera más específica. Los programadores a menudo lo hacen para limpiar datos de entrada antes de usarlos o para eliminar caracteres no deseados de una cadena.

## Cómo:

En Gleam, podemos usar la función ```delete_from_string``` para lograr este objetivo.

```Gleam
import gleam/string

let my_string = "¡Hola mundo!"
let cleaned_string = gleam/string.delete_from_string(my_string, "¡")

// El resultado será "Hola mundo!"
```

También podemos usar expresiones regulares para encontrar y eliminar caracteres específicos de una cadena.

```Gleam
import gleam/regex

let my_string = "¡Hola mundo!"
let cleaned_string = regex.replace(my_string, ~r/¡/, "")

// El resultado será "Hola mundo!"
```

## Inmersión profunda

¿Sabías que la eliminación de caracteres que coinciden con un patrón ha existido por mucho tiempo? En el pasado, los programadores solían hacerlo con lenguajes de programación como Perl o Python. Sin embargo, con el auge de lenguajes funcionales como Gleam, ahora podemos usar un enfoque más declarativo para lograrlo.

Además, existen diferentes formas de lograr este resultado. Mientras que en Gleam podemos usar la función ```delete_from_string``` o expresiones regulares, en otros lenguajes podemos encontrar funciones específicas como ```strip()``` en Python o ```trim()``` en PHP.

En cuanto a la implementación en Gleam, la función ```delete_from_string``` es simplemente un envoltorio alrededor de la función ```replace``` del módulo ```gleam/std``` que se encarga de eliminar los caracteres específicos de una cadena.

## Ver también

Si deseas saber más sobre cómo trabajar con cadenas de texto en Gleam, puedes consultar la documentación del módulo ```gleam/string```. También puedes aprender más sobre expresiones regulares en la guía de Gleam sobre manejo de patrones.

¡Eso es todo por ahora! Espero que este artículo te haya proporcionado una breve pero útil introducción sobre cómo borrar caracteres que coinciden con un patrón en Gleam. ¡Hasta la próxima!