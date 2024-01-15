---
title:                "Comparando dos fechas"
html_title:           "Clojure: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Clojure?

La comparación de dos fechas puede ser útil en varios casos, como en la programación de sistemas de reserva, planificación de tareas y análisis de datos temporales. En Clojure, podemos hacer esto fácilmente utilizando algunas funciones integradas.

## Cómo comparar dos fechas

Para comparar dos fechas en Clojure, podemos utilizar la función `compare` de la librería `java.time`. Esta función toma dos parámetros de tipo `LocalDate` y devuelve un número entero que representa la posición relativa de las fechas.

```Clojure
(require '[java.time :as time])

;; Definimos dos fechas
(def fecha-1 (time/LocalDate/of 2020 6 1))
(def fecha-2 (time/LocalDate/of 2020 6 15))

;; Comparamos las fechas
(time/compare fecha-1 fecha-2)

;; Output: -1 (fecha-1 es anterior a fecha-2)
```

El valor de salida de `compare` es:

- `-1` si la primera fecha es anterior a la segunda.
- `0` si ambas fechas son iguales.
- `1` si la primera fecha es posterior a la segunda.

También podemos utilizar las funciones `before?` y `after?` para comprobar si una fecha es anterior o posterior a otra.

```Clojure
(time/before? fecha-1 fecha-2)

;; Output: true (fecha-1 es anterior a fecha-2)

(time/after? fecha-1 fecha-2)

;; Output: false (fecha-1 es anterior a fecha-2)
```

Además, podemos utilizar la función `between?` para comprobar si una fecha está entre dos fechas dadas.

```Clojure
(time/between? fecha-1 fecha-2 (time/LocalDate/of 2020 6 10))

;; Output: true (fecha-1 está entre fecha-2 y 2020-06-10)
```

## Profundizando en la comparación de fechas

La librería `java.time` también proporciona algunas funciones para realizar operaciones matemáticas en fechas, como `plus`, `minus` y `with`.

```Clojure
(time/plus fecha-1 (time/Period/ofDays 5))

;; Output: 2020-06-06 (fecha-1 + 5 días)

(time/minus fecha-1 (time/Duration/ofDays 10))

;; Output: 2020-05-22 (fecha-1 - 10 días)

(time/with fecha-1 (time/ChronoField/$mon (time/DayOfWeek/THURSDAY)))

;; Output: 2020-06-04 (fecha-1 ajustada al jueves)
```

Para obtener más información sobre las funciones y tipos de datos relacionados con fechas en Clojure, puedes revisar la documentación oficial de `java.time` y la librería `clj-time`.

## Ver también

- [Documentación oficial de java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Librería clj-time](https://github.com/clj-time/clj-time)