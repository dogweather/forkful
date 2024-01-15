---
title:                "Eliminando caracteres que coincidan con un patrón"
html_title:           "Clojure: Eliminando caracteres que coincidan con un patrón"
simple_title:         "Eliminando caracteres que coincidan con un patrón"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, cuando trabajas con cadenas de texto en tus aplicaciones de Clojure, puede ser útil eliminar ciertos caracteres que coinciden con un patrón específico. Por ejemplo, puede que desees eliminar todos los signos de puntuación de un texto, o todas las vocales de una palabra. Afortunadamente, en Clojure existe una función fácil de usar para lograr esto.

## Cómo hacerlo

El primer paso es utilizar la función `clojure.string/replace` y pasarle tres argumentos: la cadena de texto en la que deseas realizar la eliminación, el patrón que deseas eliminar y una cadena vacía como reemplazo. Aquí hay un ejemplo de cómo eliminar todas las vocales de una palabra:

```Clojure
(clojure.string/replace "hola mundo" #"[aeiou]" "")
```
Este código devuelve la cadena "hl mnd". El patrón `#"[aeiou]"` busca todas las vocales y las reemplaza con una cadena vacía.

También puedes utilizar expresiones regulares para hacer coincidir patrones más complejos. Por ejemplo, si deseas eliminar todos los signos de puntuación y espacios de una frase, puedes usar el siguiente patrón: `#"[[:punct:][:space:]]"`. Aquí hay un ejemplo de cómo hacerlo:

```Clojure
(clojure.string/replace "¡Hola, mundo!" #"[[:punct:][:space:]]" "")
```
Este código devuelve la cadena "Hola mundo".

## Profundizando

Además de la función `clojure.string/replace`, también puedes utilizar la función `clojure.string/replace-first` si sólo deseas eliminar la primera coincidencia del patrón en lugar de todas las coincidencias. Otra función útil es `clojure.string/trim` que elimina todos los espacios en blanco al principio y al final de una cadena.

Si deseas eliminar caracteres específicos en lugar de un patrón, puedes utilizar la función `clojure.string/replace` con la función `clojure.string/includes?` para verificar si un carácter está presente en una cadena antes de reemplazarlo.

## Ver también

- Documentación oficial de Clojure sobre la función `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Expresiones regulares en Clojure: https://clojure.org/reference/regular_expressions