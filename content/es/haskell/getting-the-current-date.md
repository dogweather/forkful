---
title:    "Haskell: Obteniendo la fecha actual"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Todos los programadores saben lo importante que es mantener la precisión del tiempo en sus aplicaciones. Ya sea para registrar eventos, generar informes o simplemente mostrar la fecha actual en una interfaz de usuario, es crucial poder obtener la fecha y hora correctas. Afortunadamente, Haskell nos ofrece una forma sencilla y eficiente de hacerlo. En esta publicación, aprenderemos cómo obtener la fecha actual en Haskell.

## Cómo hacerlo

Usando la biblioteca `time` de Haskell, podemos acceder fácilmente a la fecha y hora actual. Primero, importamos el módulo en la parte superior de nuestro archivo:

```Haskell
import Data.Time
```

Luego, podemos utilizar la función `getCurrentTime` para obtener la fecha y hora actuales en un objeto `UTCTime`:

```Haskell
currentDateTime <- getCurrentTime
```

Podemos imprimir el resultado utilizando la función `print` y formatear el objeto `UTCTime` utilizando la función `formatTime`:

```Haskell
print (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentDateTime)
```

Este código imprimirá la fecha y hora actual en el formato `AAAA-MM-DD HH:MM:SS` en la consola.

También podemos obtener la fecha actual en diferentes zonas horarias utilizando la función `utcToLocalTime` y especificando la zona horaria deseada. Por ejemplo, para obtener la fecha actual en Nueva York, podemos hacer lo siguiente:

```Haskell
easternDateTime <- utcToLocalTime (hoursToTimeZone (-5)) currentDateTime
```

## Profundizando

Además de obtener la fecha actual, la biblioteca `time` también nos permite realizar operaciones con fechas, como sumar o restar un cierto número de días o meses. También podemos obtener diferentes componentes de la fecha, como el día, el mes y el año, utilizando las funciones `toGregorian`, `toOrdinalDate` y `toWeekDate`, entre otras.

Para obtener información más detallada sobre cómo trabajar con fechas en Haskell, se recomienda consultar la documentación de la biblioteca `time`.

## Ver también

- [Documentación de la biblioteca `time` de Haskell](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Guía de formatos de fecha y hora en Haskell](https://wiki.haskell.org/Date_formatting_and_parsing)