---
title:                "Generación de números aleatorios"
html_title:           "Clojure: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Generar números aleatorios es un proceso común en la programación en el que se obtienen valores numéricos aleatorios. Los programadores lo utilizan para simular situaciones aleatorias, como lanzar un dado o seleccionar una opción al azar entre varias posibles.

## Cómo hacerlo:
```Clojure
; Generar un número aleatorio entre 0 y 10
(rand-int 10)
; Output: 5

; Generar un número aleatorio entre 1 y 100
(rand 100)
; Output: 48.22315007870651

; Generar una lista de 5 números aleatorios entre 1 y 20
(repeat 5 #(rand-int 20))
; Output: (10 7 18 5 2)
```

## Profundizando:
En la historia de la computación, la generación de números aleatorios ha sido un desafío constante, ya que las máquinas son determinísticas y no pueden generar verdaderos números aleatorios. Por esta razón, se han desarrollado diferentes algoritmos para obtener resultados que se comporten de manera pseudoaleatoria. En Clojure, se utilizan las funciones `rand` y `rand-int` para generar números aleatorios, pero existen otras alternativas como Random.org que utiliza verdaderos generadores de números aleatorios externos.

## Ver también:
- La documentación oficial de Clojure sobre la generación de números aleatorios: https://clojure.org/reference/java_interop#_other_jvm_randoms
- Un artículo sobre cómo usar Random.org con Clojure: https://medium.com/@egdons/value-random-org-in-clojure-58f42fd8ac6d
- Ejemplos de uso de la función `rand-int`: https://clojuredocs.org/clojure.core/rand-int