---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Gleam: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

En ocasiones, cuando estamos escribiendo código en Gleam, nos encontramos con que necesitamos eliminar ciertos caracteres que coinciden con un patrón en particular. Ya sea para limpiar datos, filtrar una cadena o simplemente por cuestiones de formato, el eliminar caracteres en Gleam puede resultar útil en muchas situaciones.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Gleam, utilizamos la función `delete_replacements()` de la biblioteca [`gleam/regex`](https://gleam.run/packages/gleam/regex). Esta función toma dos argumentos: la cadena original y el patrón a eliminar.

Veamos un ejemplo práctico de cómo implementar esto en nuestro código:

```Gleam
import gleam/regex

let mensaje = "¡Hola, *Gleam* es increíble!"

let mensaje_sin_formato = regex.delete_replacements(mensaje, "*Gleam*")

// El resultado debería ser "¡Hola,  es increíble!"
```

Como podemos ver, en nuestro código importamos la biblioteca `gleam/regex` y luego utilizamos la función `delete_replacements()` para eliminar el patrón `*Gleam*` de nuestra cadena `mensaje`. Luego, almacenamos el resultado en una nueva variable llamada `mensaje_sin_formato` y el resultado final será "¡Hola, es increíble!".

Podemos utilizar esta función con cualquier patrón que queramos eliminar en nuestra cadena. Además, también podemos utilizar expresiones regulares para patrones más complejos.

## Profundizando

La función `delete_replacements()` utiliza la función `replace_all()` de la biblioteca `gleam/regex` y simplemente reemplaza todas las coincidencias con una cadena vacía. Esto significa que cualquier patrón que podamos utilizar en `replace_all()`, también podemos utilizarlo en `delete_replacements()`.

Por ejemplo, si queremos eliminar todos los dígitos de una cadena en Gleam, podemos utilizar la expresión regular `[0-9]` como patrón en `delete_replacements()` y todos los dígitos serán eliminados de nuestra cadena.

## Ver también

- [Documentación de la biblioteca `gleam/regex`](https://gleam.run/packages/gleam/regex)
- [Tutorial de expresiones regulares en Gleam](https://gist.github.com/lydiarrrw/3f429334470445bbbf1171438fe12a26)