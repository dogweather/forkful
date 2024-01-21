---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:05.855450-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Generar números aleatorios es como lanzar un dado digital: te da un valor impredecible. Los programadores usamos esto para todo, desde simulaciones hasta juegos y seguridad informática.

## Cómo:
```Clojure
;; Importamos la librería
(require '[clojure.java.io :as io])

;; Generamos un número aleatorio
(rand)

;; Si quieres un número entero entre 0 y 99
(rand-int 100)

;; Para obtener una secuencia de 5 números aleatorios
(repeatedly 5 #(rand-int 100))
```
Ejemplo de salida:
```
;; Un número aleatorio
0.945309405158957

;; Un número entero aleatorio entre 0 y 99
42

;; Una secuencia de 5 números enteros aleatorios
(57 11 86 14 93)
```

## Profundización
Los números aleatorios han sido una herramienta desde los tiempos de las primeras computadoras. Históricamente, se usaba hardware específico para generar aleatoriedad, pero ahora confiamos en algoritmos como Mersenne Twister para simulaciones o el algoritmo de Fortuna para criptografía.

En Clojure, `(rand)` y `(rand-int)` son genéricos y sirven para muchos propósitos, pero no son criptográficamente seguros. Para seguridad, usarías algo como `java.security.SecureRandom`.

Hay alternativas, como `rand-nth` para obtener un elemento al azar de una colección o tirar tus propios dados con combinaciones de funciones para casos más específicos.

## Ver También
- Documentación oficial de Clojure sobre números aleatorios: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand
- Una explicación más técnica de generación de números aleatorios en la programación: https://en.wikipedia.org/wiki/Random_number_generation
- Artículo sobre seguridad y números aleatorios en programación: https://www.owasp.org/index.php/Using_the_Java_SecureRandom_class