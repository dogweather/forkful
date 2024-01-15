---
title:                "Comparando dos fechas"
html_title:           "Haskell: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Haskell?

Comparar dos fechas es una tarea común en la programación, especialmente en el desarrollo de aplicaciones que manejan eventos o transacciones en diferentes momentos. En Haskell, la comparación de fechas puede ser útil para realizar operaciones lógicas y cálculos de tiempo.

## Cómo hacerlo en Haskell

Para comparar dos fechas en Haskell, podemos utilizar el tipo de dato `Day` de `Data.Time`, que representa una fecha en el calendario gregoriano. Primero, necesitaremos importar este módulo:

```Haskell
import Data.Time
```

Luego, podemos crear dos variables de tipo `Day`, asignándoles fechas específicas:

```Haskell
let fecha1 = fromGregorian 2020 5 5
let fecha2 = fromGregorian 2020 8 10
```

Y ahora, podemos comparar las fechas utilizando los operadores de comparación (`<`, `>`, `<=` y `>=`):

```Haskell
fecha1 < fecha2 -- devuelve True
fecha1 > fecha2 -- devuelve False
fecha1 <= fecha2 -- devuelve True
fecha1 >= fecha2 -- devuelve False
```

También es posible realizar comparaciones entre fechas que tengan una precisión mayor, como horas o incluso milisegundos.

## Profundizando en la comparación de fechas en Haskell

En Haskell, cuando comparamos dos fechas utilizando los operadores mencionados anteriormente, lo que realmente estamos comparando son los números enteros que representan los días. Por ejemplo, la función `fromGregorian` toma tres argumentos enteros para construir una fecha.

Por lo tanto, para fechas exactamente iguales, su comparación devolverá `False`, ya que se están comparando los días enteros y no las fechas exactas.

Sin embargo, podemos utilizar la función `compare` para obtener un resultado más específico de la comparación entre dos fechas. Esta función devolverá uno de los siguientes valores:

- `LT`: si el primer argumento es anterior al segundo.
- `GT`: si el primer argumento es posterior al segundo.
- `EQ`: si ambos argumentos son iguales.

Por ejemplo:

```Haskell
compare fecha1 fecha2 -- devuelve LT
```

También podemos utilizar la función `diffDays` para calcular la diferencia en días entre dos fechas, lo que puede ser útil para realizar cálculos de tiempo.

## Ver también

- [Documentación oficial de Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Una guía básica de Haskell para principiantes](https://programacionconhaskell.com/guia-haskell-principiantes/)
- [Cómo manejar fechas y horas en Haskell](https://tech.fpcomplete.com/haskell/tutorial/datetime)