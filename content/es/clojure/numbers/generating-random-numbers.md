---
title:                "Generación de números aleatorios"
aliases:
- /es/clojure/generating-random-numbers/
date:                  2024-01-27T20:33:03.540409-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Generar números aleatorios en la programación trata sobre crear valores que no pueden ser predichos lógicamente con antelación. Los programadores hacen esto por una variedad de razones, incluyendo la generación de identificadores únicos, la simulación de escenarios en desarrollo de juegos o la selección de muestras aleatorias de datos para análisis.

## Cómo hacerlo:

En Clojure, la generación de números aleatorios es sencilla, y hay un par de funciones incorporadas que se pueden usar de inmediato.

Para generar un número de punto flotante aleatorio entre 0 (inclusive) y 1 (exclusivo), puedes usar la función `rand`:

```Clojure
(rand)
;; Ejemplo de salida: 0.7094245047062917
```

Si necesitas un entero dentro de un rango específico, usa `rand-int`:

```Clojure
(rand-int 10)
;; Ejemplo de salida: 7
```

Esto te da un entero aleatorio entre 0 (inclusive) y el número que pasas como argumento (exclusivo).

Para generar un número aleatorio dentro de un rango específico (no limitado a enteros), puedes combinar `rand` con aritmética:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Uso
(rand-range 10 20)
;; Ejemplo de salida: 14.857457734992847
```

Esta función `rand-range` devolverá un número de punto flotante aleatorio entre los valores `min` y `max` que especificas.

Para escenarios que requieren distribuciones más complejas o secuencias de números aleatorios donde la repetibilidad es necesaria (usando semillas), es posible que necesites investigar bibliotecas adicionales que van más allá de lo que está incorporado.

## Inmersión Profunda

El mecanismo subyacente para generar números aleatorios en la mayoría de los lenguajes de programación, incluido Clojure, generalmente se basa en un generador de números pseudoaleatorios (PRNG por sus siglas en inglés). Un PRNG utiliza un algoritmo para producir una secuencia de números que aproxima las propiedades de los números aleatorios. Vale la pena señalar que, debido a que estos se generan algorítmicamente, no son verdaderamente aleatorios pero pueden ser suficientes para la mayoría de los propósitos prácticos.

En los primeros días de la computación, generar números aleatorios de alta calidad era un desafío significativo, lo que llevó al desarrollo de varios algoritmos para mejorar la aleatoriedad y la distribución. Para Clojure, las funciones incorporadas, como `rand` y `rand-int`, son convenientes para el uso diario y cubren un amplio espectro de casos de uso comunes.

Sin embargo, para aplicaciones que requieren seguridad criptográfica o métodos de muestreo estadístico más complejos, los desarrolladores de Clojure a menudo recurren a bibliotecas externas que ofrecen PRNGs más robustos y especializados. Bibliotecas como `clj-random` brindan acceso a una variedad más amplia de algoritmos y mayor control sobre la semilla, lo que puede ser crucial para simulaciones, aplicaciones criptográficas o cualquier dominio donde la calidad y previsibilidad de la secuencia de números aleatorios podría tener implicaciones significativas.

Aunque las capacidades incorporadas de Clojure para generar números aleatorios son adecuadas para muchas tareas, explorar bibliotecas externas puede ofrecer insights más profundos y opciones para aplicaciones a medida o más críticas.
