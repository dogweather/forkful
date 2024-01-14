---
title:    "Clojure: Calculando una fecha en el futuro o pasado"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado es una tarea muy común en la programación. Puede ser útil para planificar eventos o llevar un registro de plazos y vencimientos. En este artículo aprenderás cómo hacerlo usando Clojure.

## Cómo hacerlo

La forma más sencilla de calcular una fecha en el futuro o en el pasado es usando la función `clj-time.core/plus`. Esta función toma dos argumentos: una fecha y una duración. La duración puede ser expresada en segundos, minutos, horas, días, semanas, meses o años. Por ejemplo, si queremos calcular la fecha de hoy más 2 días, podemos escribir:

```Clojure
(def fecha (clj-time.core/today))
(def fecha_futura (clj-time.core/plus fecha (clj-time.core/days 2)))
```

La función `clj-time.core/today` nos da la fecha actual y luego usamos `clj-time.core/days` para especificar que queremos agregar 2 días. También podemos usar operadores matemáticos para hacer cálculos más complejos. Por ejemplo, si queremos restar 3 meses de la fecha actual, podemos escribir:

```Clojure
(def fecha_pasada (clj-time.core/minus fecha (clj-time.core/months 3)))
```

Como puedes ver, la función `clj-time.core/minus` funciona de manera similar a la función `clj-time.core/plus`, pero en lugar de agregar una duración, resta una duración especificada.

## Profundizando

La librería Clj-Time utiliza la librería Joda-Time de Java para trabajar con fechas y duraciones. Esto significa que podemos aprovechar todas las funcionalidades de Joda-Time en nuestro código de Clojure. Por ejemplo, podemos usar la función `joda-time.ChronoUnit/between` para calcular la diferencia entre dos fechas en una unidad de tiempo específica, como días, semanas o años.

```Clojure
(def fecha_hoy (joda-time.LocalDate/now))
(def fecha_cumple (joda-time.LocalDate/of 1990 3 27))
(def años_de_vida (joda-time.ChronoUnit/years between fecha_cumple fecha_hoy))
```

También podemos hacer operaciones más avanzadas, como encontrar la fecha más cercana a otra fecha. Para esto, podemos usar la función `joda-time.Days/parse`, que toma una cadena de fecha como argumento y la convierte en un objeto `joda-time.LocalDate`. Luego, usando `joda-time.Days/daysBetween`, podemos encontrar la diferencia en días entre dos fechas.

## Ver también

- [Documentación de Clj-Time](https://github.com/clj-time/clj-time/wiki)
- [Documentación de Joda-Time](https://www.joda.org/joda-time/index.html)
- [Ejemplos de uso de Joda-Time en Clojure](http://www.tech.snathan.org/tech/2008/05/06/using-joda-time-in-clojure.html)