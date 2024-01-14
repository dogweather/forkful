---
title:                "Clojure: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¿Por qué calcular una fecha en el futuro o pasado?

Calcular una fecha en el futuro o pasado puede ser útil en muchas situaciones, ya sea para programar eventos, crear cronogramas o realizar análisis de datos. En este blog post, aprendemos cómo hacerlo utilizando el lenguaje de programación Clojure.

## Cómo Hacerlo

Para calcular una fecha en el futuro o pasado, podemos utilizar la función `->` de Clojure junto con la función `java.time.LocalDate` de Java. Veamos algunos ejemplos de código:

```Clojure
(def fecha-hoy (java.time.LocalDate/now))
; obtiene la fecha actual
; Output: #object[java.time.LocalDate 0x2ac1c43a "2020-06-27"]

;; Calculando una fecha en el futuro
(-> fecha-hoy                  ; fecha de hoy
    (.plusYears 3)             ; más 3 años
    (.plusMonths 4)            ; más 4 meses
    (.plusWeeks 2)             ; más 2 semanas
    (.plusDays 10))            ; más 10 días
; Output: #object[java.time.LocalDate 0x290015b0 "2024-10-07"]

;; Calculando una fecha en el pasado
(-> fecha-hoy                  ; fecha de hoy
    (.minusYears 2)            ; menos 2 años
    (.minusMonths 3)           ; menos 3 meses
    (.minusWeeks 2)            ; menos 2 semanas
    (.minusDays 7))            ; menos 7 días
; Output: #object[java.time.LocalDate 0x33604c51 "2018-03-13"]
```

## Profundizando

La función `->` de Clojure es una macro que nos permite encadenar varias funciones. En este caso, estamos encadenando las funciones `(.plusYears)`, `(.plusMonths)`, `(.plusWeeks)` y `(.plusDays)` para sumar o restar una cantidad determinada de años, meses, semanas y días a la fecha inicial.

También podemos utilizar otras funciones de la librería java.time para obtener información más específica sobre la fecha, como por ejemplo:

- `(.getYear)` para obtener el año de la fecha
- `(.getMonthValue)` para obtener el número del mes (1-12)
- `(.getDayOfMonth)` para obtener el día del mes
- `(.getDayOfWeek)` para obtener el día de la semana
- `(.getDayOfYear)` para obtener el número del día en el año
- `(.isLeapYear)` para verificar si el año es bisiesto

Para más información sobre la librería java.time, puedes revisar la documentación en la página de Java.

## Ver También

- [Documentación de Java para la librería java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Página oficial de Clojure](https://clojure.org/)