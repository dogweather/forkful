---
title:                "Encontrar la longitud de una cadena"
html_title:           "Clojure: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
La longitud de una cadena es simplemente la cantidad de caracteres que contiene, incluyendo espacios y signos de puntuación. Los programadores buscan encontrar la longitud de una cadena para poder manipular y analizar datos de manera efectiva.

## Cómo:
Para encontrar la longitud de una cadena en Clojure, podemos utilizar la función `count` que devuelve el número de elementos en una colección.

```Clojure
(count "¡Hola Mundo!") ; devuelve 12
```

Si queremos obtener la longitud de una cadena sin contar los espacios, podemos utilizar la función `replace` para reemplazar los espacios con una cadena vacía y luego contar los caracteres restantes.

```Clojure
(count (replace "¡Hola Mundo!" #" " "")) ; devuelve 10
```

## Inmersión Profunda:
En el pasado, encontrar la longitud de una cadena era una tarea más compleja ya que los lenguajes de programación no tenían una función dedicada para esto. Los programadores tenían que utilizar métodos como iteraciones o bucles para contar la longitud de una cadena.

En Clojure, también podemos usar la función `str` para convertir la cadena en una secuencia y luego contar la longitud usando la función `count`. Sin embargo, este enfoque no es tan eficiente como el uso de `count` directamente.

Otra opción es utilizar la función `seq` para convertir la cadena en una secuencia y luego utilizar la función `into` para agregarla a un conjunto vacío y contar la longitud de ese conjunto. Nuevamente, este enfoque no es tan eficiente como el uso de `count` directamente.

## Ver también:
- [Documentación oficial de Clojure sobre count](https://clojuredocs.org/clojure.core/count)
- [Guía práctica de Clojure](https://practicalli.github.io/clojure/strings-counting-and-manipulation.html#counting-the-length-of-a-string) para encontrar la longitud de una cadena.