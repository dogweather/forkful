---
title:    "Clojure: Comparando dos fechas"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar fechas es una tarea común en la programación, especialmente cuando se trabaja con datos que contienen información temporal. Puede ser útil para filtrar datos, realizar cálculos o simplemente para verificar si una fecha es anterior o posterior a otra. En este artículo, aprenderemos cómo comparar dos fechas en Clojure y profundizaremos en cómo funciona esta operación.

## Cómo hacerlo

En Clojure, podemos usar la función `compare` para comparar dos fechas. Esta función toma dos argumentos, las fechas a comparar, y devuelve un valor numérico que indica su relación. Si la primera fecha es anterior a la segunda, el valor será -1. Si son iguales, será 0 y si la primera fecha es posterior a la segunda, será 1.

Veamos un ejemplo de cómo usar `compare` para comparar dos fechas:

```Clojure
(let [fecha1 (java.util.Date. 2020 11 5)
      fecha2 (java.util.Date. 2020 11 10)]
  (println (compare fecha1 fecha2)))
```

El resultado de este código sería `-1`, ya que la fecha1 es anterior a la fecha2. Si cambiamos el año de la fecha1 a 2021, el resultado sería `1`, ya que ahora la fecha1 es posterior a la fecha2.

## Profundizando

Cuando comparamos fechas en Clojure, es importante tener en cuenta el formato en el que están siendo representadas. En nuestro ejemplo anterior, utilizamos el constructor `java.util.Date` para crear las fechas, pero también podríamos haber utilizado diferentes formatos, como `java.time.LocalDate` o `clj-time.core/date-time`.

Otro aspecto importante a tener en cuenta es que la función `compare` utiliza una lógica similar al operador de comparación `<` en otros lenguajes, por lo que si queremos verificar si dos fechas son iguales, en realidad deberíamos utilizar `=`.

Además, Clojure también cuenta con la librería `clj-time` que ofrece funciones más avanzadas para trabajar con fechas, como por ejemplo `before?` y `after?` que nos permiten verificar si una fecha es anterior o posterior a otra.

## Ver también

- Documentación oficial de `compare`: https://clojuredocs.org/clojure.core/compare
- Documentación oficial de `clj-time`: https://github.com/clj-time/clj-time