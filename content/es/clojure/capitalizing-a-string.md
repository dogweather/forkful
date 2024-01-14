---
title:                "Clojure: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por qué

Capitalizar una cadena es una operación común en la programación que permite cambiar el formato de un texto para que la primera letra de cada palabra esté en mayúscula. Esto puede ser útil para presentar un título o un encabezado de manera más legible para el usuario.

## Cómo hacerlo

En Clojure, podemos capitalizar una cadena utilizando la función `capitalize`. Esta función toma como argumento una cadena y devuelve una nueva cadena con la primera letra en mayúscula y el resto en minúscula.

```Clojure
(capitalize "hola mundo")  ; devuelve "Hola mundo"
(capitalize "123abc") ; devuelve "123abc"
```

Si queremos capitalizar solo la primera letra de una cadena, podemos usar la función `capitalize-first`.

```Clojure
(capitalize-first "abc") ; devuelve "Abc"
```

También podemos capitalizar todas las palabras de una cadena utilizando la función `clojure.string/capitalize`, que se encuentra en la biblioteca `clojure.string`.

```Clojure
(require '[clojure.string :as str])
(str/capitalize "hola mundo") ; devuelve "Hola Mundo"
```

## Profundizando

Hay varios enfoques para capitalizar una cadena en Clojure. Además de las funciones mencionadas anteriormente, también podemos usar la función `clojure.string/replace` para buscar y reemplazar cada palabra por sí misma capitalizada.

```Clojure
(str/replace "hola mundo" #"\w+" #(clojure.string/capitalize %)) ; devuelve "Hola Mundo"
```

Otra opción es utilizar el módulo `clojure.string.capitalize` de la biblioteca `clojure.string` para capitalizar una cadena a través de una macro que envuelve todas las palabras en una expresión `capitalize`.

```Clojure
(use 'clojure.string.capitalize)
(hoja "hola mundo") ; devuelve "Hola Mundo"
```

En resumen, hay varias formas de capitalizar una cadena en Clojure, cada una con sus propias ventajas y desventajas en términos de eficiencia y legibilidad del código.

## Ver también

- [Documentación oficial de Clojure sobre la función `capitalize`](https://clojuredocs.org/clojure.string/capitalize)
- [Ejemplos de capitalización de cadenas en Clojure](https://gist.github.com/gphilipp/3510392)
- [Otras funciones útiles de la biblioteca `clojure.string`](https://www.braveclojure.com/core-functions-in-depth/#string)