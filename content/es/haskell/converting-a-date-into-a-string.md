---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Haskell: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Si estás programando en Haskell, es posible que en algún momento necesites convertir una fecha en una cadena de texto. Esto puede ser útil, por ejemplo, para imprimir una fecha en un formato específico o para almacenarla en una base de datos.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Haskell, puedes usar la función "formatTime" de la librería Data.Time. Esta función toma como argumentos un formato de fecha y la fecha que quieres convertir. Veamos un ejemplo:

```Haskell
import Data.Time
main = do
    let fecha = fromGregorian 2020 9 20 -- representa el 20 de septiembre de 2020
        cadena = formatTime defaultTimeLocale "%d/%m/%Y" fecha
    putStrLn cadena
```

El resultado de este código sería "20/09/2020". En este caso, estamos usando el formato "%d/%m/%Y" que especifica que queremos la fecha en el formato día/mes/año. Puedes jugar con diferentes formatos y fechas para obtener el resultado deseado.

## Profundizando

Si quieres saber más sobre cómo trabajar con fechas y convertirlas en cadenas de texto en Haskell, puedes consultar la documentación de Data.Time en la [página oficial de Haskell](https://www.haskell.org/hoogle/?hoogle=Time). Allí encontrarás más información y ejemplos sobre cómo manejar fechas en Haskell.

## Ver también

- [Cómo trabajar con tiempos y fechas en Haskell](https://wiki.haskell.org/Working_with_time)
- [Documentación oficial de Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Ejemplos de uso de formatTime](https://www.stackage.org/haddock/lts-10.0/time-1.8.0.2/Data-Time-Format.html#v:formatTime)