---
title:                "Clojure: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón?

Borrar caracteres que coinciden con un patrón es una acción común en la programación para limpiar y formatear datos de manera eficiente.

## Cómo hacerlo:

```Clojure
(def string "hola123")
(re-seq #"[0-9]" string)
```

El código anterior utiliza la función `re-seq` para encontrar y eliminar todos los números en la cadena "hola123". El patrón utilizado, `[0-9]`, indica a la función que busque cualquier número del 0 al 9 en la cadena. La salida sería `("1" "2" "3")`, lo que significa que los números en la cadena han sido eliminados.

## Profundizando:

La función `re-seq` utiliza expresiones regulares para encontrar una coincidencia con un patrón dado en una cadena. Utilizando diferentes patrones, es posible eliminar múltiples tipos de caracteres de una cadena, como letras, símbolos o espacios en blanco.

Además, existen otras funciones en Clojure que también permiten borrar caracteres basados en patrones, como por ejemplo la función `re-find` que devuelve la primera coincidencia encontrada en lugar de todas las coincidencias como lo hace `re-seq`.

## Ver también:

- [Documentación de Clojure sobre expresiones regulares](https://clojure.org/reference/regular_expressions)
- [Tutorial de expresiones regulares en Clojure](https://www.brainbell.com/tutorials/Clojure/Regular_Expressions.html)
- [Ejemplos de expresiones regulares en Clojure](https://gist.github.com/shastri65/f9a4781997b8e9313493)