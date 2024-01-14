---
title:                "Clojure: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por qué generar números aleatorios en Clojure?

Generar números aleatorios es una habilidad esencial de cualquier programador. A menudo, necesitamos crear datos aleatorios para probar nuestro código o para aplicaciones de juegos y sorteos. En Clojure, hay varias formas de generar números aleatorios y en esta publicación, exploraremos algunas de ellas.

## Cómo hacerlo

Para generar números aleatorios en Clojure, usaremos la función `rand`, que devuelve un número aleatorio entre 0 y 1. Podemos usar esta función para generar un número aleatorio entero entre un rango específico utilizando la función `rand-int`. Por ejemplo:

```
;; Genera un número aleatorio entre 1 y 10
(rand-int 10)
;; Resultado: 7
```

También podemos generar un número aleatorio en un rango específico utilizando la función `rand-range`. Esta función toma dos parámetros, el inicio y el fin del rango. Por ejemplo:

```
;; Genera un número aleatorio entre 5 y 15
(rand-range 5 16)
;; Resultado: 10
```

Otra forma de generar números aleatorios en Clojure es utilizando la biblioteca `clojure.math.numeric-tower`. Esta biblioteca nos proporciona varias funciones para trabajar con números, incluyendo `random-int` y `random-float`. Por ejemplo:

```
;; Genera un número aleatorio entre 1 y 100
(clojure.math.numeric-tower/random-int 100)
;; Resultado: 54

;; Genera un número aleatorio entre 0 y 1
(clojure.math.numeric-tower/random-float)
;; Resultado: 0.7844042781559317
```

## Profundizando

Ahora que sabemos cómo generar números aleatorios en Clojure, veamos cómo funciona esta función en sí misma. En realidad, las funciones `rand`, `rand-int` y `rand-range` utilizan la función `java.util.Random` de Java para generar los números aleatorios. Esta clase utiliza un algoritmo de generación de números pseudoaleatorios llamado "Linear Congruential Generator". Este algoritmo tiene una semilla que se alimenta a través de una serie de operaciones matemáticas para producir una secuencia aparentemente aleatoria de números.

Es importante tener en cuenta que, aunque estos números son aleatorios en términos matemáticos, no son verdaderamente aleatorios, ya que se generan a través de un algoritmo. Por lo tanto, no deben utilizarse para fines criptográficos o de seguridad.

# Ver también

- Documentación de Clojure sobre la función `rand`: https://clojuredocs.org/clojure.core/rand
- Documentación de Clojure sobre la función `rand-int`: https://clojuredocs.org/clojure.core/rand-int
- Documentación de Clojure sobre la función `rand-range`: https://clojuredocs.org/clojure.core/rand-range