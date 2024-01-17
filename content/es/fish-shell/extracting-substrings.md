---
title:                "Extracción de subcadenas"
html_title:           "Fish Shell: Extracción de subcadenas"
simple_title:         "Extracción de subcadenas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Extraer subcadenas, o pedazos de texto más pequeños dentro de un texto más grande, es una tarea común en la programación. Los programadores lo hacen para obtener información específica de una cadena de texto más grande, como una dirección de correo electrónico o un número de teléfono.

## ¡Cómo hacerlo!

Para extraer una subcadena en la terminal de Fish Shell, utilizamos el comando `string sub` seguido de la cadena original, el índice inicial y el número de caracteres que queremos extraer. Por ejemplo:

```
Fish Shell $ string sub "Hola mundo" 2 4
Salida → "ola "
```
En este ejemplo, estamos extrayendo una subcadena de la cadena "Hola mundo" comenzando en el segundo índice (que corresponde a la letra "o") y extrayendo 4 caracteres.

También podemos especificar el índice final en lugar de la longitud de la subcadena. Por ejemplo:

```
Fish Shell $ string sub "123456789" 2 -4
Salida → "234"
```

## Profundizando

Extraer subcadenas es una técnica común en la programación moderna, utilizada en muchos lenguajes de programación diferentes. Sin embargo, algunos lenguajes pueden tener métodos específicos para realizar esta tarea, como el método `substring` en JavaScript.

Si bien `string sub` es la forma más sencilla de extraer subcadenas en la terminal de Fish Shell, también existen otras formas de lograrlo. Por ejemplo, podemos utilizar el comando `awk` para extraer una subcadena específica basada en un patrón de búsqueda.

Con respecto a la implementación, el comando `string sub` utiliza el método `substr` en el núcleo de GNU para extraer la subcadena deseada. Esto garantiza una ejecución eficiente y precisa.

## Ver también

- Documentación oficial de Fish Shell sobre el comando `string sub`: https://fishshell.com/docs/current/cmds/string.html#sub
- Artículo sobre el método `substring` en JavaScript: https://www.w3schools.com/jsref/jsref_substring.asp
- Documentación oficial de GNU sobre el método `substr`: https://www.gnu.org/software/gawk/manual/html_node/String-Functions.html