---
title:                "Haskell: Obteniendo la fecha actual."
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces cuando estamos programando, necesitamos saber la fecha y hora actual. Ya sea para mostrarla en nuestra aplicación, para almacenarla en una base de datos o simplemente para usarla en alguna lógica de negocio. En Haskell, obtener la fecha actual es muy sencillo y en este post te explicaré cómo hacerlo.

## Cómo hacerlo

Para obtener la fecha actual en Haskell, utilizamos la librería `time` y su módulo `Data.Time`. Primero, debemos importar este módulo en nuestro código:

```Haskell
import Data.Time
```

Una vez que tenemos la librería importada, podemos usar la función `getCurrentTime` para obtener la fecha y hora actual en formato `UTCTime`. Con esta función, podemos crear un objeto `ZonedTime` que nos da la fecha y hora local en nuestra zona horaria. El código sería así:

```Haskell
currentDate <- getCurrentTime
localDate <- utcToLocalTime <$> getTimeZone <*> pure currentDate
```

Para mostrar la fecha en un formato más legible, podemos usar la función `formatTime` con el formato deseado como primer argumento y el `ZonedTime` como segundo. Por ejemplo, si queremos mostrar la fecha en formato `día/mes/año`, el código sería:

```Haskell
dateString <- formatTime defaultTimeLocale "%d/%m/%Y" localDate
```

Y si queremos mostrar también la hora, podemos usar el formato `día/mes/año %H:%M:%S`. Por último, podemos imprimir el resultado usando la función `putStrLn`:

```Haskell
putStrLn dateString
```

La salida sería algo así: `10/04/2021 14:08:36`

## Profundizando

Ahora que sabemos cómo obtener la fecha actual en Haskell, es importante mencionar que cuando usamos la función `getCurrentTime`, estamos obteniendo la fecha y hora en formato `UTCTime`. Esto es una importante diferencia con otros lenguajes de programación como Java, donde obtenemos por defecto la fecha y hora en nuestra zona horaria local.

Si queremos trabajar con la fecha y hora en nuestra zona horaria local en Haskell, debemos usar la función `getCurrentLocaleTime`, que nos da directamente un objeto `ZonedTime` con la fecha y hora en nuestra zona horaria.

## Ver también

- [Documentación de la librería Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial de Haskell: fechas y tiempo](http://dmalcolm.fedorapeople.org/presentations/HaskellDatesAndTimes.pdf)