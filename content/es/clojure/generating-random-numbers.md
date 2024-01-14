---
title:                "Clojure: Generando números aleatorios"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios?

Generar números aleatorios es una técnica muy útil en la programación ya que permite simular comportamientos aleatorios o tomar decisiones al azar. También puede ser una buena herramienta para crear juegos o aplicaciones que requieren de cierta imprevisibilidad.

## Cómo hacerlo

En Clojure, podemos crear números aleatorios utilizando la función `rand` de la librería `clojure.core`. Esta función devuelve un número decimal entre 0 y 1. Si queremos un número entero, podemos utilizar la función `rand-int`.

```Clojure
;; Generar un número aleatorio decimal
(rand)

;; Generar un número aleatorio entero entre 0 y 10
(rand-int 10)
```

También podemos utilizar la función `rand-nth` para obtener un elemento aleatorio de una lista.

```Clojure
;; Crear una lista de números
(def numeros [1 2 3 4 5])

;; Obtener un elemento aleatorio de la lista
(rand-nth numeros)
```

## Profundizando en la generación de números aleatorios

Es importante mencionar que la función `rand` utiliza un generador de números pseudoaleatorios, lo que significa que los números no son realmente aleatorios y se basan en un algoritmo determinístico. Esto puede ser útil en ciertos casos, pero si necesitamos una verdadera aleatoriedad, podemos utilizar la función `secure-random` de la librería `clojure.java.io`.

```Clojure
;; Generar un número aleatorio seguro
(clojure.java.io/secure-random)
```

Otra técnica interesante es utilizar semillas o seeds para generar una secuencia de números pseudoaleatorios. Esto nos permite obtener la misma secuencia cada vez que ejecutamos el código con la misma semilla.

```Clojure
;; Generar una secuencia de números aleatorios con una semilla
(rand-nth (take 5 (repeatedly #(rand-int 10) 1234)))
```

## Ver también

- [Funciones rand y rand-int](https://clojuredocs.org/clojure.core/rand)
- [Función rand-nth](https://clojuredocs.org/clojure.core/rand-nth)
- [Función secure-random](https://clojuredocs.org/clojure.java.io/secure-random)