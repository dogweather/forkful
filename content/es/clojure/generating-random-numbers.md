---
title:    "Clojure: Generando números aleatorios"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Clojure?

Generar números aleatorios es una habilidad crucial en la programación, ya que permite a los desarrolladores crear aplicaciones y programas más dinámicos e interactivos. En Clojure, es fácil generar números aleatorios y utilizarlos en una variedad de aplicaciones.

## Cómo hacerlo:

Generar un número aleatorio en Clojure es sencillo usando la función ```(rand)```, que devuelve un número decimal aleatorio entre 0 y 1. Si se quiere generar un número entero, se puede utilizar la función ```(rand-int n)```, donde n es el número máximo que se quiere generar.

Un ejemplo sencillo sería generar un número aleatorio entre 1 y 10:

```Clojure
(rand-int 10) ;; output: 5
```

También se puede utilizar la función ```(repeat n f)```, donde n es el número de veces que se quiere generar un número aleatorio y f es la función que se quiere aplicar a cada número generado. Por ejemplo, si se quiere generar 5 números aleatorios entre 1 y 100, se podría usar la siguiente expresión:

```Clojure
(repeat 5 #(rand-int 100)) ;; output: (71 14 89 40 5)
```

## Profundizando:

Si se quiere generar números aleatorios en un rango específico, se puede utilizar la función ```(rand-nth coll)```, donde coll es una colección de valores en el rango deseado. Por ejemplo, si se quiere generar un número aleatorio entre 10 y 20, se puede crear una lista con esos valores y luego usar ```(rand-nth)```:

```Clojure
(def rand-range (range 10 21)) ;; creando una lista con los valores del 10 al 20
(rand-nth rand-range) ;; output: 17
```

Además, si se necesita generar números aleatorios con una distribución específica, se puede utilizar la librería ```clojure.math.numeric-tower``` y su función ```(rand-nth coll)```.

## Ver También:

- [Documentación oficial de Clojure sobre números aleatorios](https://clojuredocs.org/clojure.core/rand)
- [Artículo sobre generación de números aleatorios en Clojure](https://pureconcepture.wordpress.com/2011/06/08/clojure-using-random-numbers/)

¡Espero que este artículo te haya ayudado a entender cómo generar números aleatorios en Clojure y a utilizarlos en tus proyectos!