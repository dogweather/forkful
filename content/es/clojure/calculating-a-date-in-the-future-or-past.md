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

## ¿Qué y por qué?
Calcular una fecha en el futuro o en el pasado es una tarea común en programación. Los programadores a menudo necesitan encontrar la fecha correspondiente a una cierta cantidad de días, semanas o meses después o antes de una fecha dada. Esto puede ser útil para programar recordatorios, planificar eventos o hacer cálculos sobre el tiempo.

## Cómo hacerlo:
Usando la biblioteca estándar de Clojure, podemos utilizar la función ```java.util.Calendar/add```, que nos permite sumar o restar una cantidad determinada de días, semanas o meses a una fecha dada. A continuación, un ejemplo de cómo obtener la fecha correspondiente a una semana después de la fecha actual:

```
Clojure (java.util.Calendar/add java.util.Calendar/DATE 7)
```

Esto producirá la siguiente salida: ```#inst "2021-07-06T20:09:05.000-00:00"```, que representa la fecha del 6 de julio de 2021.

## Profundizando:
En el pasado, los desarrolladores tenían que manejar manualmente los cálculos de fechas y horas, lo que podía ser tedioso y propenso a errores. Sin embargo, con el avance de la tecnología y el uso de bibliotecas como la mencionada anteriormente, ahora es una tarea simple y eficiente.

Otras alternativas para calcular fechas en Clojure incluyen usar la librería ```clj-time``` o realizar los cálculos utilizando funciones de bajo nivel como ```java.util.Date``` o ```java.time.LocalDate``` de Java.

Detrás de escena, la función ```java.util.Calendar/add``` utiliza el método ```add()``` del objeto ```java.util.Calendar``` de Java para calcular la fecha deseada.

## Ver también:
- Documentación oficial de Clojure sobre la función ```java.util.Calendar/add```: https://clojuredocs.org/clojure.java/javadocs/java.util.Calendar/add
- Documentación de la librería ```clj-time```: https://github.com/clj-time/clj-time