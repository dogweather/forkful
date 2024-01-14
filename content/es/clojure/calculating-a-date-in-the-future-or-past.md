---
title:                "Clojure: Calculando una fecha en el futuro o pasado"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Calcular fechas en el futuro o en el pasado es una tarea común en la programación. Puede ser útil para manejar eventos futuros, programar tareas o simplemente para obtener una mejor comprensión del tiempo.

## Cómo hacerlo

```Clojure
(require '[clj-time.core :as time])

;; Obtener la fecha actual
(def fecha-actual (time/today))  ;; 2021-04-12

;; Calcular una fecha en el futuro 
(def fecha-futura (time/plus fecha-actual (time/days 7)))  ;; 2021-04-19

;; Calcular una fecha en el pasado
(def fecha-pasada (time/minus fecha-actual (time/days 14)))  ;; 2021-03-29
```

## Profundizando

Calcular fechas en el futuro o en el pasado requiere conocimiento sobre cómo funcionan las fechas en informática. En Clojure, podemos utilizar la biblioteca `clj-time` para facilitar el manejo de fechas y tiempos. Esta biblioteca utiliza la clase `java.time` de Java para proporcionar funciones y macros para crear, comparar y manipular fechas.

Algunas de las funciones útiles incluyen `time/today` para obtener la fecha actual, `time/plus` para sumar un cierto número de días, meses o años a una fecha y `time/minus` para restarlos.

También es importante tener en cuenta que cuando trabajamos con fechas pasadas o futuras, es importante considerar diferentes zonas horarias y cambios en el horario de verano. La biblioteca `clj-time` maneja automáticamente estos cambios, lo que hace que el proceso de cálculo de fechas sea más confiable y preciso.

## Ver también

- Documentación oficial de `clj-time`: https://github.com/clj-time/clj-time
- Tutorial de cálculo de fechas con `clj-time`: https://www.baeldung.com/clojure-clj-time
- Práctica de cálculo de fechas en Elixir: https://elixirschool.com/es/lessons/basics/basics/time/