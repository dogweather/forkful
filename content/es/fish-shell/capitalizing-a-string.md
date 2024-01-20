---
title:                "Capitalizando una cadena de texto"
html_title:           "Fish Shell: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra en una cadena a mayúsculas. Los programadores a menudo lo hacen para mejorar la legibilidad o para normalizar/formato de entradas de usuario.

## Cómo hacerlo:

En Fish Shell, podemos usar la función `string` para esto.

```Fish Shell
set cadena "en un lugar de la mancha"

set capitalizado (string split " " $cadena | string upper | string join ' ')

echo $capitalizado
```
Salida de muestra:

```Fish Shell
EN UN LUGAR DE LA MANCHA
```

## Inmersión profunda:

Históricamente, la capitalización de cadenas ha sido un problema común en la programación y cada lenguaje tiende a tener su propia manera de manejarlo. En Fish Shell, utilizamos la funcionalidad incorporada en la función `string`. Con la ayuda de `split`, `upper` y `join`, somos capaces de capitalizar cada palabra de una cadena.

Existen alternativas a la función `string`, pero requieren más líneas de código y no son necesariamente más eficientes o legibles. 

El `string split " "` divide la cadena en palabras, `string upper` convierte todas las palabras resultantantes a mayúsculas y `string join ' '` las une de nuevo.

## Consulte también:

Para más detalles, consulte la documentación oficial de Fish Shell para la función `string` aquí: https://fishshell.com/docs/current/cmds/string.html 

Para más detalles sobre los conceptos de capitalización y cómo se maneja en diferentes idiomas de programación, este enlace es bastante útil: https://rosettacode.org/wiki/Capitalize_words_in_a_string

Para una comparación más detallada de las características de los lenguajes de programación con respecto la manejo de cadenas, puede visitar: https://rosettacode.org/wiki/String_Manipulation