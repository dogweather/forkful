---
title:    "Clojure: Encontrando la longitud de una cadena"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué

En programación, es común necesitar saber el tamaño o longitud de una cadena de texto. Esto puede ser útil para validar entradas de usuarios, contar caracteres o realizar operaciones de manipulación de cadenas. En este artículo explicaremos cómo encontrar la longitud de una cadena en Clojure.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Clojure, podemos utilizar la función `count`. Esta función devuelve el número de elementos de una secuencia, y en el caso de una cadena, cuenta el número de caracteres.

```Clojure
(count "Hola Mundo") ; Devuelve 10
(count "") ; Devuelve 0
```

También podemos usar `str` para convertir una cadena en una secuencia explícita y luego contarla.

```Clojure
(count (str "¡Hola!" " " "¿Cómo estás?")) ; Devuelve 15
```

## Profundizando

Hay algo importante que tener en cuenta cuando se trabaja con cadenas en Clojure. En realidad, las cadenas están representadas como secuencias de caracteres Unicode, por lo que cuando usamos `count` estamos contando el número de elementos en esa secuencia, no el número de caracteres visibles.

Por ejemplo, si tenemos una cadena que contiene un emoji, `count` contará el emoji como un solo elemento en lugar de dos caracteres.

```Clojure
(count "¡Hola! 😊") ; Devuelve 8
```

Si queremos contar el número de caracteres visibles, podemos utilizar `clojure.string/length`, que tiene en cuenta los caracteres Unicode para obtener un recuento más preciso.

```Clojure
(require '[clojure.string :as str])
(str/length "¡Hola! 😊") ; Devuelve 6
```

## Ver también

- [Documentación oficial de count en Clojure](https://clojuredocs.org/clojure.core/count)
- [Documentación oficial de clojure.string/length](https://clojuredocs.org/clojure.string/length)
- [Artículo de tutorial de Clojure de FreeCodeCamp](https://www.freecodecamp.org/news/writing-clojure-scripts-to-auto-prefix-commit-messages-3e82731f7c18/)