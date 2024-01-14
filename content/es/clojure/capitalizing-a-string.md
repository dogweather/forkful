---
title:    "Clojure: Capitalizando una cadena"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena de texto es una tarea común en la programación. A menudo, necesitamos mostrar títulos o nombres en mayúscula y saber cómo hacerlo en Clojure puede ahorrar tiempo y evitar errores.

## Cómo hacerlo

Para capitalizar una cadena en Clojure, podemos utilizar la función `clojure.string/capitalize` que tomará una cadena como argumento y devolverá una nueva cadena con la primera letra en mayúscula.

```Clojure
(clojure.string/capitalize "hola mundo")
;; Output: "Hola mundo"

(clojure.string/capitalize "el coche rojo")
;; Output: "El coche rojo"
```

Si necesitamos convertir toda la cadena a mayúsculas, podemos usar `clojure.string/upper-case`.

```Clojure
(clojure.string/upper-case "hola mundo")
;; Output: "HOLA MUNDO"

(clojure.string/upper-case "el coche rojo")
;; Output: "EL COCHE ROJO"
```

Finalmente, si queremos convertir toda la cadena a minúsculas, usamos `clojure.string/lower-case`.

```Clojure
(clojure.string/lower-case "HOLA MUNDO")
;; Output: "hola mundo"

(clojure.string/lower-case "EL COCHE ROJO")
;; Output: "el coche rojo"
```

## Profundizando

En realidad, la función `clojure.string/capitalize` no solo convierte la primera letra de la cadena a mayúscula, sino que también convierte el resto de la cadena a minúsculas. Por ejemplo:

```Clojure
(clojure.string/capitalize "hOlA mUnDo")
;; Output: "Hola mundo"
```

Esto significa que no podemos usar esta función para capitalizar cadenas que ya contienen mayúsculas y minúsculas de forma específica. En este caso, debemos escribir nuestra propia función personalizada que solo convierta la primera letra a mayúscula sin afectar al resto de la cadena.

Otra consideración importante es que la función `clojure.string/capitalize` solo funciona para cadenas con caracteres ASCII. Si necesitamos capitalizar cadenas con caracteres no ASCII, podemos usar la biblioteca `java.text.Normalizer` y su método `java.text.Normalizer/normalize`.

## Ver También

- [Documentación oficial de Clojure para la función `clojure.string/capitalize`](https://clojuredocs.org/clojure.string/capitalize)
- [Tutorial de Clojure de Codecademy que cubre la función `clojure.string/capitalize`](https://www.codecademy.com/learn/learn-clojure/modules/learn-clojure-functions/cheatsheet)
- [Otra forma de capitalizar cadenas en Clojure usando la biblioteca `java.lang.String`](https://stackoverflow.com/questions/15119307/capitalizing-the-first-letter-of-every-word-in-a-string-using-clojure)
- [Documentación oficial de Java para la clase `java.text.Normalizer`](https://docs.oracle.com/javase/7/docs/api/java/text/Normalizer.html)