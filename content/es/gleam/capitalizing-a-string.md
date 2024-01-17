---
title:                "Capitalizando una cadena"
html_title:           "Gleam: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

¡Hola, queridos programadores! ¿Están buscando una manera rápida y efectiva de capitalizar cadenas de texto en sus aplicaciones? Si es así ¡no busquen más! En este artículo de programación, les enseñaré cómo capitalizar una cadena de texto en el lenguaje de programación Gleam. ¿Están listos? ¡Comencemos!

## ¿Qué y por qué?

Capitalizar una cadena de texto simplemente significa convertir la primera letra de cada palabra en mayúscula. Los programadores lo hacen para mejorar la legibilidad y presentación de la información en sus aplicaciones. Además, al seguir convenciones de estilo y formato, se aseguran de que el código sea fácil de entender y mantener para ellos y otros desarrolladores.

## ¿Cómo hacerlo?

Para capitalizar una cadena de texto en Gleam, hay dos opciones: la función `String.capitalize/1` o la macro `String.capitalize_words/1`. La función `String.capitalize/1` toma una cadena de texto y devuelve una nueva cadena de texto con la primera letra en mayúscula. La macro `String.capitalize_words/1` toma una cadena de texto y devuelve una nueva cadena de texto con todas las palabras en mayúscula.

```Gleam
import gleam/string

my_string = "hola mundo"
capitalized_string = String.capitalize(my_string)
/* ¡El resultado es "Hola mundo"! */

cap_words = String.capitalize_words(my_string)
/* El resultado es "Hola Mundo" */
```

## Profundizando

Si bien capitalizar una cadena de texto puede parecer una tarea simple, tiene un contexto histórico importante. Antes de Gleam, el lenguaje de programación Erlang utilizaba `atom`, una estructura de datos que puede contener cadenas de texto, para representar identificadores y nombres de funciones. Sin embargo, `atom` tenía un límite de 255 caracteres y no admitía mayúsculas y minúsculas. Esto obligó a los programadores a usar `string` en su lugar, lo que resultó en una gran cantidad de código repetitivo para capitalizar cadenas de texto. Gleam solucionó este problema al proporcionar funciones y macros integradas para capitalizar y manipular cadenas de texto fácilmente.

Si bien la función `String.capitalize/1` y la macro `String.capitalize_words/1` son las opciones más comunes para capitalizar cadenas de texto en Gleam, también existen otras alternativas, como crear una función personalizada utilizando el módulo `gleam/char` o utilizando bibliotecas externas como `gleam/unicode`. Sin embargo, es importante tener en cuenta que estas alternativas pueden no ser tan eficientes o confiables como las funciones y macros integradas de Gleam.

## Ver también

Si desean profundizar aún más en la manipulación de cadenas de texto en Gleam, pueden consultar la documentación oficial de Gleam o explorar la vasta comunidad en línea que comparte recursos y ejemplos útiles. ¡Hasta la próxima, programadores! ¡Continúen escribiendo código limpio y eficiente con Gleam!