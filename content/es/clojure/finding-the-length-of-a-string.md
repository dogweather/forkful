---
title:    "Clojure: Encontrando la longitud de una cadena"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

La programación en Clojure puede ser una actividad emocionante y enriquecedora, y una de las tareas básicas que todo programador debe saber es cómo encontrar la longitud de una cadena de texto. Descubre cómo con esta guía paso a paso.

## Cómo Hacerlo

Para encontrar la longitud de una cadena en Clojure, utilizaremos la función `count()`. Esta función toma cualquier colección - incluyendo una cadena - y devuelve el número de elementos en ella.

Veamos un ejemplo práctico:

```Clojure
(def str "¡Hola, mundo!")
(count str)
```

La salida de este código será `13`, que es la longitud de la cadena "¡Hola, mundo!". Como se puede ver, la función `count()` es muy fácil de usar y nos permite encontrar rápidamente la longitud de cualquier cadena.

También podemos utilizar esta función con listas, vectores, mapas y otros tipos de colecciones. Por ejemplo:

```Clojure
(def lista [1 2 3 4 5])
(count lista) ; Output: 5

(def mapa {"edad" 30, "género" "masculino"})
(count mapa) ; Output: 2
```

¡Incluso podemos usar `count()` en una cadena de texto vacía! En ese caso, su resultado será `0`.

Una cosa importante a tener en cuenta es que `count()` es una función de orden constante, lo que significa que su tiempo de ejecución no depende del tamaño de la colección. Esto lo hace muy eficiente y recomendado para su uso en proyectos de gran escala.

## Deep Dive

Ahora que sabemos cómo encontrar la longitud de una cadena en Clojure, puede que nos preguntemos cómo funciona en realidad esta función. En realidad, `count()` no cuenta los caracteres en la cadena, sino que utiliza la función `.count()` del propio objeto para obtener su longitud. Esta función es implementada por todas las colecciones en Clojure, lo que explica por qué podemos usarla en diferentes tipos de datos.

Otro aspecto interesante es que `count()` es una función polimórfica, lo que significa que puede tomar diferentes tipos de colecciones como argumentos. En resumen, `count()` es una herramienta poderosa y versátil que nos permite encontrar la longitud de cualquier colección en Clojure de manera rápida y eficiente.

## Ver También

- [Documentación oficial de count en Clojure](https://clojuredocs.org/clojure.core/count)
- [Clojure for the Brave and True - Contando colecciones](http://www.braveclojure.com/core-functions-in-depth/#Counting_Collections)
- [Learn Clojure.org - Funciones de colección en Clojure](https://www.learn-clojure.org/basics/collection-functions/)