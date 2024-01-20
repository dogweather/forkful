---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La generación de números aleatorios se refiere a crear números que no siguen un patrón predecible. Los programadores lo hacen para añadir elementos de azar y evitar la repetición.

## ¿Cómo hacerlo?

Para generar un número aleatorio en Clojure, la función que necesitas es `rand`.

```Clojure
;; Generar un número aleatorio entre 0 y 1
(rand)
```

Este código produce un número aleatorio entre 0 y 1. Si quieres un número aleatorio dentro de un rango específico, puedes utilizar `rand-int`.

```Clojure
;; Generar un número aleatorio de 0 a 10
(rand-int 10)
```

Este código produce un número aleatorio de 0 a 10.

## Profundización

Históricamente, los sistemas de generación de números aleatorios han sido de vital importancia en la simulación, la criptografía y otros campos. En Clojure, los números aleatorios se generan a partir de la clase `java.util.Random`.

Existen otras alternativas para generar números aleatorios en Clojure. Por ejemplo, puedes usar `java.security.SecureRandom` para obtener números aleatorios más seguros adecuados para la criptografía.

```Clojure
;; Generar un número aleatorio seguro
(def secure-random (java.security.SecureRandom.))
(.nextBytes secure-random (byte-array 16))
```

Debajo del capó, `rand` y `rand-int` en Clojure utilizan `java.util.Random`. Esta clase genera números aleatorios siguiendo un algoritmo de suma de desplazamiento, lo que proporciona una secuencia de números pseudoaleatorios.

## También puedes ver

Para más información sobre la generación de números aleatorios en Clojure, puedes consultar las siguientes fuentes:

- [Documentación de Clojure](https://clojure.org/)
- [Tutoriales de Clojure](https://www.learn-clojurescript.com/)