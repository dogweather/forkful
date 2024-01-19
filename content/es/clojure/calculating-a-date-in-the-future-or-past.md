---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Clojure: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Calcular una fecha futura o pasada es determinar exactamente un día, mes y año futuro o previo desde una fecha dada. Los programadores lo hacen para manejar tareas basadas en el tiempo, como registrar eventos, programar pagos, etc.

## Como hacerlo:

Clojure te permite manipular fechas con facilidad. Dado un objeto de fecha, puedes añadir o restar días, meses o años.

```Clojure
(require '[clj-time.core :as t]
         '[clj-time.coerce :as c]
         '[clj-time.periodic :as p])

(defn future-date [days months years]
  (-> 
   (t/now)
   (t/plus (t/years years))
   (t/plus (t/months months))
   (t/plus (t/days days))
   (c/to-date))) 

(defn past-date [days months years]
  (-> 
   (t/now)
   (t/minus (t/years years))
   (t/minus (t/months months))
   (t/minus (t/days days))
   (c/to-date)))
```

Estas funciones producirán una fecha futura o pasada, respectivamente, en función de la cantidad de días, meses y años que añadas o restes.

## Inmersión Profunda:

A lo largo de la historia de la programación, los desarrolladores han empleado varios métodos para el cálculo de fechas. Un enfoque popular es usar bibliotecas de manejo de tiempo, como Joda-Time en Java. Clojure, un dialecto de Lisp que corre en la JVM, tiene acceso a estas bibliotecas, pero también ofrece su propio manejo idiomático de tiempo a través de `clj-time`. 

Una alternativa a `clj-time` sería `java.util.Date` y `java.util.Calendar`, pero ya están obsoletos en Java y son menos flexibles en comparación.

Detalles de implementación: en las funciones anteriores, usamos `t/now` para obtener la fecha/hora actual y `t/plus` o `t/minus` para calcular fechas futuras o pasadas. Las funciones `t/years`, `t/moons` y `t/days` devuelven períodos, que son sumados o restados de la fecha/hora actual.

## Ver ademas:

Para aprender más sobre el manejo del tiempo en Clojure, revisa los siguientes recursos:

- [ClojureDocs, clj-time](https://clojuredocs.org/clojure.core/time)
- [Clj-time en Github](https://github.com/clj-time/clj-time)
   
Para entender las diferencias entre `java.util.Date`, `java.util.Calendar` y Joda-Time, visita:

- [Joda-time vs java.util.Date](https://www.baeldung.com/java-date)
- [Java 8, Date Time API overview](https://www.baeldung.com/java-8-date-time-intro)