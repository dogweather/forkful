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

## Por qué

Generar números aleatorios es una habilidad esencial para cualquier programador, ya sea para juegos, simulaciones o pruebas aleatorias. Además, puede ser una forma divertida de aprender y practicar programación en Clojure.

## Cómo hacerlo

La función `rand` de Clojure nos permite generar un número aleatorio entre 0 y 1. Podemos usarlo junto con otros operadores para obtener un rango específico de números aleatorios. Por ejemplo, para generar un número entero aleatorio entre 1 y 10, podemos usar `(+ 1 (rand-int 10))`. Aquí hay un ejemplo completo de código que genera 5 números enteros aleatorios entre 1 y 100 y los imprime en la consola:

```Clojure
(defn generar-numeros [cantidad]
  (dotimes [_ cantidad]
    (println (+ 1 (rand-int 100)))))
    
(generar-numeros 5)
```

La salida de este código podría ser algo como:

```
28
75
42
11
96
```

## Profundizando

Las funciones `rand` y `rand-int` utilizan un algoritmo llamado "Mersenne Twister" para generar números pseudoaleatorios. Esto significa que los números aparentemente aleatorios que obtenemos son en realidad el resultado de una serie de cálculos matemáticos predecibles. Sin embargo, para la mayoría de los casos prácticos, estos números funcionan de manera similar a los números verdaderamente aleatorios.

Si necesitamos una semilla específica para generar una secuencia de números aleatorios repetibles, podemos proporcionarla como argumento en `rand` o `rand-int`. Además, Clojure ofrece otras funciones para generar tipos específicos de datos aleatorios, como `rand-nth` para elegir un elemento aleatorio de una secuencia, o `shuffle` para reordenar una secuencia de manera aleatoria.

## Véase también

- [Documentación de Clojure sobre funciones para generar números aleatorios](https://clojuredocs.org/clojure.core/rand)
- [Una introducción a la programación en Clojure](https://www.pragmaticprogrammer.com/titles/shcloj3/programming-in-clojure-third-edition.pdf)