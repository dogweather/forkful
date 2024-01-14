---
title:    "Clojure: Generando números aleatorios"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## ¿Por Qué Generar Números Aleatorios?

Generar números aleatorios es una técnica común en la programación para simular situaciones aleatorias o para generar datos aleatorios. Esto puede ser útil en juegos, pruebas, análisis de datos y más. En este artículo, exploraremos cómo podemos generar números aleatorios en el lenguaje de programación Clojure.

## Cómo Generar Números Aleatorios

La función `rand` en Clojure nos permite generar un número aleatorio entre 0 y 1. Podemos utilizar la función `rand-int` para generar un número entero aleatorio dentro de un rango específico. Aquí hay un ejemplo de código:

```Clojure
;; Generar un número aleatorio entre 0 y 1
(rand)

;; Generar un número entero aleatorio entre 1 y 10
(rand-int 10)
```

La salida de estos ejemplos puede variar cada vez que se ejecute debido a la naturaleza aleatoria de la función `rand`.

Podemos generar múltiples números aleatorios utilizando la función `repeatedly`. Por ejemplo, si queremos generar 5 números aleatorios enteros entre 1 y 100, podemos escribir el siguiente código:

```Clojure
(repeatedly 5 #(rand-int 100))
```

La función `repeatedly` ejecutará la función anónima `#(rand-int 100)` 5 veces y nos devolverá una lista con los resultados.

## Profundizando en la Generación de Números Aleatorios

Es importante tener en cuenta que la función `rand` utiliza un generador de números pseudoaleatorios. Esto significa que los números generados no son totalmente aleatorios, sino que siguen un patrón determinado por el generador utilizado.

En Clojure, podemos especificar un generador de números aleatorios utilizando la función `set!`. También podemos utilizar la función `seed` para establecer una semilla para nuestro generador, lo que nos permitirá obtener la misma secuencia de números aleatorios cada vez que ejecutamos nuestro código.

Otra función útil para la generación de números aleatorios en Clojure es `shuffle`. Esta función nos permite mezclar una lista de manera aleatoria. Aquí hay un ejemplo de cómo podemos utilizarla:

```Clojure
(shuffle [1 2 3 4 5])
```

La salida podría ser algo como `[4 1 2 5 3]`.

## Ver También

- Documentación oficial de Clojure sobre la función `rand`: https://clojuredocs.org/clojure.core/rand
- Tutorial sobre generación de números aleatorios en Clojure: https://www.tutorialspoint.com/clojure/clojure_random_numbers.htm
- Generación de números aleatorios en juegos con Clojure: https://medium.com/@N3rdf1ght3r/building-a-text-adventure-game-in-clojure-part-1-random-numbers-and-boolean-logic-7f403f2f47f0