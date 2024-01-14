---
title:                "Haskell: Comparando dos fechas"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas puede ser una tarea común en la programación, especialmente al trabajar con datos temporales. En Haskell, existen varias formas de comparar fechas, cada una con sus propias ventajas y aplicaciones. En este artículo, exploraremos cómo comparar dos fechas en Haskell.

## Cómo hacerlo

Para comparar dos fechas en Haskell, podemos utilizar la función `compare` de la biblioteca `Data.Time` de Haskell. Esta función toma dos valores de tipo `UTCTime` (que representan fechas y horas en formato UTC) y devuelve un valor de tipo `Ordering` que indica si la primera fecha es anterior, igual o posterior a la segunda fecha.

Tomemos como ejemplo dos fechas hipotéticas, "10 de abril de 2021" y "15 de mayo de 2021". Primero, importemos la biblioteca `Data.Time`:

```Haskell
import Data.Time
```

Luego, definamos dos valores `UTCTime` con las fechas mencionadas:

```Haskell
let fecha1 = UTCTime 2021 4 10 0 0 0
let fecha2 = UTCTime 2021 5 15 0 0 0
```

Finalmente, utilicemos la función `compare` para comparar las dos fechas:

```Haskell
compare fecha1 fecha2
```

El resultado que obtendremos es `GT`, que significa "mayor que", indicando que la primera fecha es posterior a la segunda.

Aparte de `compare`, también existen otras funciones en la biblioteca `Data.Time` que nos permiten comparar fechas, como `(<)`, `(<=)`, `(>)` y `(>=)`. Estas se pueden utilizar de manera similar a `compare`.

## Inmersión profunda

Además de comparar fechas, también podemos realizar otras operaciones con ellas, como sumar o restar una cantidad de tiempo determinada. Por ejemplo, si tenemos una fecha y queremos sumarle 3 días, podemos utilizar la función `addDays` de la biblioteca `Data.Time`:

```Haskell
addDays 3 fecha1
```

El resultado será una nueva fecha que represente la fecha original más 3 días adicionales.

Otra forma de comparar fechas en Haskell es utilizando la biblioteca `Data.Time.Calendar` y su función `diffDays`. Esta función toma dos valores `Day` (que representan fechas sin la hora) y devuelve la diferencia en días entre las dos fechas.

```Haskell
import Data.Time.Calendar

let fecha1 = fromGregorian 2021 4 10
let fecha2 = fromGregorian 2021 5 15

diffDays fecha1 fecha2
```

El resultado en este caso sería `35`, indicando que hay 35 días de diferencia entre las dos fechas.

En resumen, comparar fechas en Haskell puede ser muy útil y hay varias formas de hacerlo dependiendo de nuestras necesidades y la biblioteca que estemos utilizando. Es importante leer la documentación de cada función para asegurarnos de utilizarla correctamente.

## Ver también

- Documentación oficial de la biblioteca `Data.Time`: https://hackage.haskell.org/package/time/docs/Data-Time.html
- Documentación oficial de la biblioteca `Data.Time.Calendar`: https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html