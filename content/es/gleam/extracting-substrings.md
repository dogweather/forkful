---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Extracción de substrings es desglosar una cadena de caracteres en segmentos más pequeños. Es útil para obtener partes específicas de datos de texto más grandes.

## Cómo hacerlo:
La extracción de substrings en Gleam se puede hacer usando la función `slice`. A continuación se muestra un ejemplo:

```Gleam
import gleam/string

let mi_cadena = "¡Hola, Mundo!"
let substring = string.slice(0, 5, mi_cadena)
```
En este ejemplo, el substring sería `"¡Hola"`.

Otro ejemplo sería:

```Gleam
let otro_substring = string.slice(7, 13, mi_cadena)
```
En este caso, el substring sería `"Mundo!"`.

## Inmersión Profunda:

1. Contexto Histórico: La funcionalidad de extracción de substrings ha sido una parte esencial de los lenguajes de programación desde sus primeros días.

2. Alternativas: Aunque 'slice' es la principal función de extracción de substrings embedida en Gleam, también se podrían usar otras funciones como `split`, dependiendo del caso.

3. Detalles de Implementación: La función `slice` en Gleam utiliza parámetros de inicio y fin para extraer substrings. Empezando desde 0, los números representan las posiciones de los caracter.

## Ver También:

Para más información y recursos relacionados, puede visitar los siguientes enlaces:

- [Gleam Docs: Slice](https://gleam.run/documentation/)
- [Gleam Discussion: String Functions](https://github.com/gleam-lang/gleam/discussions)