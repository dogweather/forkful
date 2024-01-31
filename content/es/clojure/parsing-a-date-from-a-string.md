---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:35:33.331669-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Parsear una fecha desde un string significa convertir texto que representa una fecha a una forma con la que el programa puede trabajar fácilmente. Lo hacemos para manipular, almacenar o comparar fechas, tareas comunes en programación.

## Cómo hacerlo:
Usaremos la biblioteca `clj-time`, basada en Joda-Time, para parsear fechas en Clojure. Primero, asegúrate de añadir la última versión de `clj-time` a tu archivo `project.clj` como dependencia. Luego, aquí te dejo un ejemplo de cómo parsear una fecha:

```clojure
(require '[clj-time.format :as fmt])
(require '[clj-time.coerce :as coerce])

;; Definimos el formato de la fecha que esperamos
(def custom-formatter (fmt/formatter "dd-MM-yyyy"))

;; Parseamos el string a un objeto Joda-Time DateTime
(defn parse-date [date-string]
  (coerce/from-string (fmt/parse custom-formatter date-string)))

;; Ejemplo de uso
(parse-date "23-03-2023")
;; => #object[org.joda.time.DateTime 0x somehash "2023-03-23T00:00:00.000Z"]
```

## Datos Detallados:
Clj-time es una envoltura Clojure alrededor de Joda-Time, la biblioteca Java estándar para fechas antes de Java 8. Java 8 introdujo `java.time`, una API de tiempo más moderna, pero clj-time sigue popular en proyectos Clojure.

Otras alternativas incluyen el uso de la biblioteca estándar de Java directamente con `java.util.Date` o `java.time`. Una implementación en Clojure podría usar funciones `clj-time.core` para manipular fechas despues de parsearlas.

Detalles de implementación importantes:
- Asegúrate de manejar zonas horarias (timezones) correctamente.
- Valida el string de la fecha antes de parsearlo para prevenir errores.
- Cuando parsees fechas, considera usar `clj-time` para una API más idiomática y funcional.

## Ver También:
- Documentación de `clj-time`: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Guía de Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
- Sobre `java.time`: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
