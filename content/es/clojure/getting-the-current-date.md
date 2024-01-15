---
title:                "Obteniendo la fecha actual"
html_title:           "Clojure: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Si alguna vez has necesitado obtener la fecha actual en tus programas, entonces este artículo es para ti. Aprenderás cómo obtener la fecha actual en Clojure y cómo puedes usarla en tus proyectos.

## Cómo hacerlo

```Clojure
(require '[java.time.LocalDate :as LocalDate])

(def today (LocalDate/now))
(def current-year (.getYear today))
(def current-month (.getMonth today))
(def current-day (.getDayOfMonth today))

(println (str "Hoy es " current-day "/" current-month "/" current-year))
```

Este código importa la librería `java.time.LocalDate` que nos permite trabajar con fechas. Luego, usamos la función `now` para obtener la fecha actual y guardamos sus valores en variables. Finalmente, imprimimos la fecha en formato día/mes/año.

## Profundizando

La función `now` en realidad está llamando a la clase `LocalDateTime`, que contiene no solo la fecha, sino también la hora actual. Si solo quieres obtener la fecha, puedes usar la función `LocalDate/now` en su lugar.

También puedes especificar una zona horaria para obtener la fecha para esa zona en particular:

```Clojure
(def now-zoned (ZonedDateTime/now (ZoneId/of "Europe/Madrid")))
(def current-date (.toLocalDate now-zoned))

(println current-date)
```

En este caso, estamos obteniendo la fecha actual para la zona horaria de Madrid. Luego, usamos la función `.toLocalDate` para obtener solo la fecha sin la hora.

## Ver también

- Documentación oficial de `java.time.LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Ejemplos de uso del paquete `java.time`: https://www.baeldung.com/java-8-date-time-intro