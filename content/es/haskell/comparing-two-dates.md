---
title:    "Haskell: Comparando dos fechas"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Haskell?

Comparar fechas es una tarea común en la programación, ya sea para verificar si una fecha es anterior o posterior a otra, o para ordenar una lista de eventos cronológicamente. En este artículo, exploraremos cómo podemos hacerlo en el lenguaje funcional Haskell y aprenderemos por qué es importante dominar esta habilidad.

## ¿Cómo hacerlo?

Antes de profundizar en la comparación de fechas en Haskell, necesitamos entender cómo se representan las fechas en este lenguaje. Haskell tiene un tipo de datos llamado `Data.Time.Day` que se usa para almacenar fechas. Para crear una fecha específica, podemos usar la función `fromGregorian` que toma tres argumentos: año, mes y día.

Veamos un ejemplo de cómo podemos comparar dos fechas en Haskell usando la función `compare`.

```Haskell
import Data.Time

-- Creamos dos fechas
fecha1 = fromGregorian 2020 5 15
fecha2 = fromGregorian 2020 5 20

-- Comparamos las fechas y almacenamos el resultado en una variable
resultado = compare fecha1 fecha2

-- Imprimimos el resultado
print resultado
```

La salida de este código será `LT`, que significa "menor que" en Haskell. Esto nos indica que la `fecha1` es anterior a la `fecha2`. Otros posibles resultados de la función `compare` incluyen `GT` (mayor que) y `EQ` (igual a).

## Un poco más profundo

En Haskell, la comparación de fechas se basa en el número de días transcurridos desde una fecha de referencia. Este número de días se conoce como el número de días julianos y está relacionado con el sistema de calendario gregoriano. Debido a esto, no podemos comparar fechas anteriores al 1 de enero de 1753.

Además, si queremos comparar también las horas y los minutos de dos fechas, podemos usar el tipo de datos `Data.Time.LocalTime` que incluye la hora del día. Podemos combinar este tipo de datos con la función `compare` para obtener una comparación más precisa de dos fechas.

## Ver también

- [Documentación oficial de Haskell sobre Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Guía de programación de Haskell en español](https://wiki.haskell.org/Espa%C3%B1ol)