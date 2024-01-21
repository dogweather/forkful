---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:45:37.460353-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Extraer subcadenas significa seleccionar partes específicas de una cadena de texto. Los programadores hacen esto para manipular y utilizar solo los fragmentos necesarios de la información.

## ¿Cómo?

```Elm
import String exposing (slice)

-- Para obtener 'mundo' de 'Hola mundo'
let
    saludo = "Hola mundo"
    mundo = slice 5 10 saludo
in
    mundo  -- "mundo"
```

```Elm
import String exposing (left, right)

-- Para obtener 'Hola' de 'Hola mundo' usando left
let
    saludo = "Hola mundo"
    hola = left 4 saludo
in
    hola  -- "Hola"

-- Para obtener 'mundo' de 'Hola mundo' usando right
let
    saludo = "Hola mundo"
    mundo = right 5 saludo
in
    mundo  -- "mundo"
```

## Profundización

Históricamente, la extracción de subcadenas ha sido una operación fundamental en la manipulación de texto en muchos lenguajes de programación. En Elm, el enfoque funcional para trabajar con cadenas promueve la inmutabilidad y la claridad del código.

Alternativas para extraer subcadenas en otros lenguajes pueden incluir el uso de expresiones regulares o métodos incorporados más especializados. En Elm, funciones como `slice`, `left`, y `right` de `String` son claras y directas para este propósito.

En cuanto a detalles de implementación, `slice` acepta dos índices, donde el primer número es inclusivo y el segundo es exclusivo. Esta función maneja adecuadamente caracteres Unicode, lo que es importante para trabajar con idiomas que tienen caracteres fuera del conjunto ASCII básico.

La utilización de funciones específicas de extracción de subcadenas asegura que el código sea más fácil de leer y mantener. Además, previene errores comunes que pueden surgir al tratar con índices directamente o al mutar cadenas.

## Véase También

- Documentación oficial de Elm sobre el módulo `String`: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm guide en español, que incluye secciones sobre manejo de strings: https://guide.elm-lang.org/es/
- Libro "Aprendiendo Elm" ("Learning Elm" en inglés) que puede proporcionar un contexto adicional sobre el lenguaje en general: https://elmprogramming.com/