---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Comparar dos fechas significa examinar dos puntos específicos en el tiempo para examinar cuál es anterior, posterior o si son idénticos. Los programadores lo hacen para tomar decisiones en sus programas, por ejemplo, para verificar si una tarea se ha excedido en tiempo.

## Cómo Hacerlo:

Veamos cómo efectivamente podemos comparar dos fechas en Clojure:

```Clojure
;; Importamos la librería de Java para trabajar con fechas.
(import java.time.LocalDate)

;; Creamos dos fechas para comparar.
(def date1 (LocalDate/of 2022 1 1))
(def date2 (LocalDate/of 2022 1 2))

;; Comparamos las fechas
(if (.isBefore date1 date2)
  (println "date1 es anterior a date2")
  (if (.isAfter date1 date2)
    (println "date1 es posterior a date2")
    (println "Las fechas son iguales")))

; Mostrará "date1 es anterior a date2"
```

## Más en Detalle:

Los programadores han estado comparando fechas desde el principio de la programación. En Clojure, normalmente utilizamos la biblioteca java.time.LocalDate para trabajar con fechas. Existen alternativas como la biblioteca clj-time si necesitas más funcionalidad.

Al comparar fechas, la implementación detallada dependerá de las peculiaridades de tu problema. Por ejemplo, si también necesitas considerar las zonas horarias, probablemente deberías usar java.time.ZonedDateTime en lugar de LocalDate.

## Ver También:

- Documentación oficial de Java para [LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html).
- Biblioteca [clj-time](https://github.com/clj-time/clj-time) en GitHub.