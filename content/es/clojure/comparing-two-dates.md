---
title:                "Clojure: Comparando dos fechas"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
Comparar fechas es una tarea común en la programación y es especialmente importante para manejar datos relacionados con el tiempo. Al aprender cómo comparar fechas en Clojure, puedes mejorar la precisión y la eficiencia de tus programas.

## Cómo hacerlo
Para comparar dos fechas en Clojure, puedes utilizar las funciones `<=`, `>=`, `<`, y `>`. Estas funciones toman dos argumentos de tipo fecha y devuelven un valor booleano que indica si la primera fecha es menor, mayor o igual a la segunda fecha.

```Clojure
(def fecha1 (java.time.LocalDate/of 2020 1 1))
(def fecha2 (java.time.LocalDate/of 2020 1 15))

(<= fecha1 fecha2)      ; devuelve true
(>= fecha1 fecha2)      ; devuelve false
(< fecha1 fecha2)       ; devuelve true
(> fecha1 fecha2)       ; devuelve false
```

También puedes utilizar la función `compare` para obtener un número que indica la relación entre las dos fechas. Si el resultado es -1, significa que la primera fecha es anterior a la segunda; si el resultado es 1, significa que la primera fecha es posterior a la segunda; y si el resultado es 0, significa que las dos fechas son iguales.

```Clojure
(compare fecha1 fecha2)    ; devuelve -1
```

## Profundizando
Al comparar fechas, es importante tener en cuenta el formato en el que están representadas. En Clojure, las fechas son objetos inmutables de la clase `java.time.LocalDate`, por lo que no se pueden modificar directamente.

Para crear una fecha, puedes utilizar la función `of` del paquete `java.time.LocalDate` y especificar el año, mes y día como argumentos numéricos. También puedes utilizar la función `parse` para crear una fecha a partir de una cadena en un formato específico.

```Clojure
(def fecha3 (java.time.LocalDate/parse "2020-03-30"))
```

Además, es importante recordar que las fechas pueden tener zonas horarias y que pueden ser afectadas por cambios en los horarios de verano. Esto puede influir en la comparación de fechas en diferentes momentos.

## Ver también
- [Documentación de java.time.LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Artículo sobre cómo trabajar con fechas en Clojure](https://www.baeldung.com/clojure-dates)
- [Tutorial de Clojure para principiantes](https://www.clojure.org/guides/getting_started)