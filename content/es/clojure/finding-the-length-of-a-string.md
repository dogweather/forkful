---
title:                "Clojure: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo necesitamos saber la longitud de una cadena de texto. Ya sea para realizar una validación o para manipular la información, conocer la longitud de una cadena es una habilidad esencial en cualquier lenguaje de programación. En este artículo, exploraremos cómo podemos encontrar la longitud de una cadena en Clojure.

## Cómo hacerlo

En Clojure, podemos usar la función `count` para obtener la longitud de una cadena. Esta función toma cualquier secuencia, incluyendo cadenas, y devuelve su longitud. Veamos un ejemplo:

```Clojure
(count "Hola") ; Devuelve 4
```

En este caso, la cadena "Hola" tiene una longitud de 4 caracteres, por lo que la función `count` devuelve 4. Podemos almacenar este valor en una variable para usarlo más tarde:

```Clojure
(def longitud (count "Hola")) ; Almacenamos el valor 4 en la variable longitud
```

También podemos usar la función `count` en combinación con la función `str`, que nos permite convertir cualquier valor en una cadena de texto. Veamos un ejemplo:

```Clojure
(count (str 123)) ; Devuelve 3
```

En este caso, la función `str` convierte el número entero 123 en una cadena de texto, y luego la función `count` devuelve su longitud, que en este caso es 3.

## Profundizando

Ahora que sabemos cómo encontrar la longitud de una cadena en Clojure, profundicemos un poco más en la función `count`. Esta función no solo funciona con cadenas, sino también con otros tipos de secuencias, como listas o vectores. Veamos un ejemplo con listas:

```Clojure
(count '(1 2 3 4 5)) ; Devuelve 5
```

También podemos usar la función `count` en una secuencia vacía, en cuyo caso devuelve 0:

```Clojure
(count []) ; Devuelve 0
```

Como ves, la función `count` es muy versátil y nos permite obtener la longitud de cualquier secuencia en Clojure de manera sencilla.

## Ver también

- [Documentación oficial de la función `count`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)
- [Tutorial de Clojure en español](https://clojuredocs.org/)

¡Espero que este artículo te haya sido útil para aprender cómo encontrar la longitud de una cadena en Clojure! Si deseas aprender más sobre este lenguaje de programación funcional, te recomiendo consultar la documentación oficial y el tutorial en español que te dejé en la sección de "Ver también". ¡Feliz programación!