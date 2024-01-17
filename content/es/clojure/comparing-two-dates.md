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

## ¿Qué es y por qué comparar dos fechas?

Comparar dos fechas es un proceso común en la programación, que consiste en determinar cuál de las dos fechas es mayor o si son iguales. Esto se utiliza para realizar cálculos de tiempo, ordenar eventos cronológicamente o para asegurarse de que un evento ocurra antes de otro.

## Cómo hacerlo:

```Clojure
(require '[clj-time.core :as time])

(def fecha1 (time/date-time 2021 1 15)) ;Definir una fecha usando la función date-time
(def fecha2 (time/date-time 2020 12 25))

(time/after? fecha1 fecha2) ;Compara si fecha1 es posterior a fecha2
;Output: true

(time/before? fecha1 fecha2) ;Compara si fecha1 es anterior a fecha2
;Output: false

(time/equal? fecha1 fecha2) ;Compara si fecha1 es igual a fecha2
;Output: false
```

## Profundizando:

En el pasado, comparar fechas era una tarea complicada que requería cálculos manuales y conversiones de formatos de fecha. Sin embargo, con el uso de librerías como clj-time, ahora es un proceso sencillo y eficiente.

Alternativas a clj-time incluyen java.time, una librería incorporada en Java 8, y Joda-Time, la versión anterior de clj-time. Estas también ofrecen funciones para comparar fechas de manera similar a clj-time.

Las fechas en Clojure se almacenan como objetos de tipo java.util.Date, que representan una cantidad de tiempo en milisegundos desde el 1 de enero de 1970. Al comparar fechas, se están comparando estos números para determinar cuál es mayor.

## También te puede interesar:

- [Guía oficial de clj-time](https://github.com/clj-time/clj-time/wiki)
- [Documentación de java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Joda-Time en Clojure](https://github.com/clj-commons/java-time)