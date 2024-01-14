---
title:    "Haskell: Calculando una fecha en el futuro o pasado"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

Calcular una fecha en el futuro o en el pasado puede ser una tarea útil en la programación, ya sea para hacer un seguimiento de eventos o para realizar cálculos relacionados con el tiempo. En este post, aprenderemos cómo realizar este tipo de cálculos usando el lenguaje de programación Haskell.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, primero necesitamos definir una función que tome como entrada una fecha y una cantidad de días, y nos devuelva la fecha resultante. En Haskell, podemos definir esta función de la siguiente manera:

```Haskell
calculateDate :: (Int, Int, Int) -> Int -> (Int, Int, Int)
calculateDate (day, month, year) days =
    let date = addDays days (fromGregorian year month day)
        (year', month', day') = toGregorian date
    in (month', day', year')
```

En la función anterior, utilizamos las funciones `addDays` y `fromGregorian` para agregar una cantidad específica de días a una fecha dada. Luego, utilizamos la función `toGregorian` para convertir la fecha resultante en una tupla de tres elementos: mes, día y año.

Ahora, podemos llamar a nuestra función `calculateDate` pasándole una fecha y la cantidad de días que queremos agregar o restar, como en el siguiente ejemplo:

```Haskell
-- Calcular una fecha 10 días después del 15 de mayo de 2021
calculateDate (15, 5, 2021) 10

-- Resultado: (5, 25, 2021)
```

También podemos utilizar números negativos para restar días, por ejemplo:

```Haskell
-- Calcular una fecha 7 días antes del 1 de enero de 2021
calculateDate (1, 1, 2021) (-7)

-- Resultado: (12, 25, 2020)
```

## Profundizando

Esta técnica de cálculo de fechas se basa en los tipos de datos `Day`, `MonthOfYear` y `Year`, que son parte del módulo `Data.Time.Calendar` de Haskell. También se utilizan funciones como `addDays` y `toGregorian` para realizar las operaciones necesarias. Puedes explorar más en profundidad estas funciones y datos en la documentación oficial de Haskell.

## Ver también

- [Documentación de `Data.Time.Calendar`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html)
- [Tutorial de Haskell](https://www.haskell.org/tutorial/)
- [Ejemplos de fechas en Haskell](https://www.stackbuilders.com/news/a-comparison-of-datetime-libraries-in-haskell)