---
title:                "Haskell: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué comparar dos fechas es importante en Haskell

Cuando se trabaja con programación en Haskell, es común tener que lidiar con datos de tipo `Date`, o fechas en español. Comprender cómo comparar dos fechas es esencial para poder realizar operaciones y tomar decisiones basadas en la fecha en la que ocurre un evento en nuestro código.

## Cómo comparar dos fechas en Haskell

La forma más sencilla de comparar dos fechas en Haskell es utilizando el operador de comparación `>`, `<`, `>=` o `<=` dependiendo de lo que se quiera comparar. Por ejemplo:

```Haskell
fecha1 = (2020, 8, 15) -- 15 de agosto de 2020
fecha2 = (2020, 8, 20) -- 20 de agosto de 2020

fecha1 > fecha2 -- False
fecha1 < fecha2 -- True
fecha1 >= fecha2 -- False
fecha1 <= fecha2 -- True
```

También podemos utilizar la función `compare` que devuelve un `Ordering` (ordenamiento) indicando si la primera fecha es menor, igual o mayor que la segunda fecha. Esta función se puede utilizar junto con la sentencia `case` para tomar diferentes acciones dependiendo del resultado de la comparación. Por ejemplo:

```Haskell
fecha1 = (2020, 8, 15) -- 15 de agosto de 2020
fecha2 = (2020, 8, 20) -- 20 de agosto de 2020

case compare fecha1 fecha2 of
  EQ -> do putStrLn "Las fechas son iguales"
  GT -> do putStrLn "La primera fecha es más reciente que la segunda"
  LT -> do putStrLn "La primera fecha es más antigua que la segunda"
```

## Profundizando en la comparación de dos fechas

Cuando comparamos dos fechas en Haskell, lo que realmente estamos comparando son dos tuplas de números enteros. En el primer ejemplo, estamos comparando la tupla `(2020, 8, 15)` con la tupla `(2020, 8, 20)`. Por lo tanto, la comparación se realiza primero entre el primer elemento de cada tupla, luego entre el segundo elemento y así sucesivamente. Esto se conoce como orden lexicográfico.

Es importante tener en cuenta que el formato de tupla utilizado para representar una fecha puede variar dependiendo de cómo se esté usando en el código. Por ejemplo, en lugar de `(año, mes, día)` se puede utilizar `(día, mes, año)` o incluso `(mes, día, año)`. Es importante asegurarse de utilizar el mismo formato en todas las comparaciones para obtener resultados coherentes.

## Ver también

- [Documentación oficial de Haskell sobre tipos de datos `Date`](https://www.haskell.org/hoogle/?hoogle=Date) 
- [Tutorial de Haskell en español sobre tipos de datos](https://andreasschrade.com/breves-2015-desde-cero-1-10-tipos-de-datos-en-haskell/)