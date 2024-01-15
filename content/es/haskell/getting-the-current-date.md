---
title:                "Obteniendo la fecha actual"
html_title:           "Haskell: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
¿Alguna vez te has preguntado cómo puedes obtener la fecha y hora actual en tus programas de Haskell? La respuesta es bastante simple, pero es una habilidad útil para tener en tu caja de herramientas de programación. En este artículo, vamos a explorar cómo obtener la fecha y hora actual en Haskell.

## How To
```Haskell
import Data.Time

main = do
  now <- getCurrentTime
  print $ show now
```

Ejecutando este código, obtendremos la fecha y hora actual en formato UTC. Si queremos cambiar el formato de salida, podemos usar la función `formatTime` del paquete `Data.Time.Format`. Por ejemplo, para mostrar la fecha y hora en el formato "DD/MM/YYYY HH:MM:SS", podemos hacer lo siguiente:

```Haskell
import Data.Time
import Data.Time.Format

main = do
  now <- getCurrentTime
  print $ formatTime defaultTimeLocale "%d/%m/%Y %H:%M:%S" now
```

Incluso podemos obtener valores específicos como el año, mes, día, hora, etc. usando funciones como `getYear`, `getMonth` o `getDay` del paquete `Data.Time.Calendar`. Este es un ejemplo de cómo obtener el mes actual en formato numérico:

```Haskell
import Data.Time.Calendar

main = do
  now <- getCurrentTime
  print $ monthNumber $ todMonth $ localDay $ utcToLocalTime utc now
  where
    monthNumber (Month _ num) = num
```

## Deep Dive
La función `getCurrentTime` devuelve un valor del tipo `UTCTime`, lo que significa que representa un momento en el espacio de tiempo universal (UTC). Para trabajar con fechas locales, podemos convertir este valor utilizando `utcToLocalTime` del paquete `Data.Time.LocalTime`. También podemos controlar el formato de salida utilizando `formatTime` y especificar el formato utilizando `defaultTimeLocale`.

## See Also
- [Documentación oficial de Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell Date and Time Tutorial](https://www.tutorialspoint.com/haskell/haskell_datetime.htm)