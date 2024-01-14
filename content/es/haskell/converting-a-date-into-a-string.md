---
title:                "Haskell: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

La conversión de una fecha en una cadena es un proceso común en la programación de Haskell. Esta tarea puede parecer trivial a simple vista, pero puede ser muy útil en situaciones donde necesitamos mostrar una fecha en un formato específico o realizar operaciones con fechas.

## Cómo hacerlo

Para convertir una fecha en una cadena, utilizamos la función `show` y le pasamos como parámetro una instancia de la clase `Data`. Por ejemplo, si queremos mostrar la fecha actual, podemos hacer lo siguiente:

```Haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    putStrLn $ show (utctDay now)
```

Este código primero importa el módulo `Data.Time`, que contiene las funciones necesarias para trabajar con fechas. Luego, utilizando `getCurrentTime`, obtenemos la fecha actual y la almacenamos en la variable `now`. Por último, aplicamos la función `show` a la fecha y la imprimimos en la consola.

La salida de este código será una cadena en el formato `AAAA-MM-DD`, por ejemplo `2021-07-15`.

## Profundizando

Existen diversas funciones y tipos de datos en el módulo `Data.Time` que nos permiten trabajar con fechas de manera más avanzada. Algunos de los más utilizados son:

- `Day`: Tipo de dato que representa una fecha en el calendario gregoriano.
- `UTCTime`: Tipo de dato que representa una fecha y hora en formato UTC.
- `parseTimeM`: Función que permite convertir una cadena en un tipo de dato `Day` o `UTCTime`.
- `formatTime`: Función que nos permite dar formato a una fecha según nuestras necesidades.

Al explorar estos y otros elementos del módulo `Data.Time`, podremos realizar operaciones más complejas y manejar fechas con mayor precisión.

## Ver también

- [Documentación de Data.Time en Hackage](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)
- [Tutorial de Haskell para principiantes](https://www.haskell.org/tutorial/)
- [Más sobre fechas y horas en Haskell](https://wiki.haskell.org/Time_and_Date_Library)