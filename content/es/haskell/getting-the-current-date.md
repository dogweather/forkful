---
title:                "Haskell: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has tenido la necesidad de saber la fecha y hora actual en un programa de Haskell? Ya sea para mostrarla a los usuarios, realizar cálculos basados en el tiempo o simplemente por curiosidad, obtener la fecha actual es una tarea común en la programación. Afortunadamente, Haskell tiene una amplia gama de funciones para manejar fechas y horas.

## Cómo hacerlo

Para obtener la fecha actual en Haskell, podemos utilizar la función `getCurrentTime` del módulo `Data.Time`. Esta función devuelve un valor del tipo `UTCTime` que contiene la fecha actual en formato UTC (Tiempo Universal Coordinado). Luego, podemos utilizar la función `getCurrentTimeZone` para obtener la zona horaria actual y, finalmente, utilizar la función `utcToLocalTime` para convertir la hora UTC a la zona horaria local.

```Haskell
import Data.Time

main = do
  currentUTC <- getCurrentTime
  timeZone <- getCurrentTimeZone
  let localTime = utcToLocalTime timeZone currentUTC
  print localTime
```

El resultado de este código sería algo como esto:

```
2021-05-20 18:55:34
```

Pero, ¿y si queremos mostrar la fecha en un formato más legible para los humanos? Para ello, podemos utilizar la función `formatTime` junto con un patrón de formato. Por ejemplo, si queremos mostrar la fecha en formato dd/mm/aaaa, podemos hacer lo siguiente:

```Haskell
import Data.Time
import Data.Time.Format

main = do
  currentUTC <- getCurrentTime
  let pattern = "%d/%m/%Y"
  let formattedLocalTime = formatTime defaultTimeLocale pattern currentUTC
  print formattedLocalTime
```

Este código nos daría el siguiente resultado:

```
20/05/2021
```

¡Genial! Ahora podemos mostrar la fecha actual en el formato que deseemos.

## Profundizando

La función `getCurrentTime` que utilizamos anteriormente es parte del módulo `Data.Time.Clock`, que a su vez utiliza el módulo `Data.Time.Clock.POSIX`. Este último módulo utiliza una implementación de la hora POSIX, que es un sistema de tiempo basado en segundos desde la época UNIX (1 de enero de 1970). La función `getCurrentTime` utiliza la hora POSIX para obtener la hora actual.

Además, si queremos trabajar con fechas específicas en lugar de la fecha actual, podemos utilizar las funciones `fromGregorian` y `localDay` para crear una fecha a partir de componentes como el día, mes y año. También podemos utilizar la función `diffDays` para calcular la diferencia de días entre dos fechas.

## Ver también

- [Documentación sobre módulo Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial de Haskell sobre fechas y horas](https://www.schoolofhaskell.com/user/edwardk/clocks-and-calendars)
- [Ejemplos de código sobre fechas y horas en Haskell](https://wiki.haskell.org/Cookbook/Date_and_time)