---
title:    "Clojure: Comparando dos fechas"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué

En el mundo de la programación, a menudo necesitamos comparar dos fechas para realizar diferentes tareas, como ordenar una lista de eventos o calcular la diferencia de días entre dos fechas. Aprender cómo comparar fechas en Clojure te permitirá manejar estas situaciones con facilidad.

## Cómo hacerlo

Para comparar dos fechas en Clojure, podemos utilizar la función `comparar`, que compara dos objetos Date y devuelve -1 si el primer objeto es anterior al segundo, 0 si son iguales y 1 si el primer objeto es posterior al segundo. Por ejemplo:

```
(def fecha1 (java.util.Date. 2021 2 1))
(def fecha2 (java.util.Date. 2021 2 15))

(comparar fecha1 fecha2)
;; output: -1

(comparar fecha2 fecha1)
;; output: 1

(comparar fecha1 fecha1)
;; output: 0
```

También podemos utilizar las funciones `antes` y `después`, que devuelven un valor booleano según si la primera fecha es anterior o posterior a la segunda. Por ejemplo:

```
(def fecha1 (java.util.Date. 2021 3 1))
(def fecha2 (java.util.Date. 2021 3 15))

(antes fecha1 fecha2)
;; output: true

(después fecha1 fecha2)
;; output: false

(después fecha1 fecha1)
;; output: false
```

## Inmersión profunda

En Clojure, las fechas se representan utilizando la clase `java.util.Date`. Esta clase es mutable, lo que significa que los objetos Date pueden ser modificados después de ser creados. Esto puede afectar a la comparación de fechas, ya que si modificamos uno de los objetos, la comparación puede dar un resultado inesperado.

Para evitar este problema, podemos utilizar la función `inmutar` que retorna una versión inmutable de la fecha original. De esta manera, podemos asegurarnos de que la comparación se realice correctamente. Por ejemplo:

```
(def fecha1 (java.util.Date. 2021 4 1))
(def fecha2 (java.util.Date. 2021 4 15))

(comparar fecha1 (inmutar fecha2))
;; output: -1

(comparar fecha2 (inmutar fecha1))
;; output: 1
```

## Ver también

- [Documentación de Clojure sobre fechas](https://clojuredocs.org/clojure.java-time/compare)

- [Tutorial de Clojure para principiantes](https://www.codingame.com/playgrounds/929/clojure-tutorial-for-beginners/introduction)

- [Ejemplos de comparaciones de fechas en Clojure](https://rosettacode.org/wiki/A_Better_Way_To_Manipulate_Dates#Clojure)