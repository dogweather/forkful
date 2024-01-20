---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Obtener la fecha actual en tu programa es básicamente pedirle a tu ordenador que te dé el día de hoy. Los programadores hacen esto para registrar cuándo ocurre un evento, para programar tareas futuras, entre otras razones.

## Cómo se hace:

Primero, hay que importar el módulo `Data.Time`. Para obtener la fecha actual, utilizamos la función `getCurrentTime`.

```Haskell
import Data.Time

main = do
    current <- getCurrentTime
    print current
```
El resultado de ejecutar este programa será la fecha y hora actuales, con formato `AAAA-MM-DD HH:MM:SS UTC`.

## Profundizando

`getCurrentTime` proviene del paquete `time`, que se agregó en GHC 6.12. Antes de eso, los programadores tenían que usar librerías del sistema operativo, que eran mucho más complicadas.

Hay otras formas de obtener la fecha actual. Por ejemplo, puedes usar `getZonedTime` para obtener la fecha y hora locales en lugar de UTC.

En cuanto a los detalles de implementación, `getCurrentTime` en realidad se comunica con tu sistema operativo para obtener la fecha actual. Dado que las fechas pueden variar dependiendo de la zona horaria, se usa UTC (Tiempo Universal Coordinado) para evitar confusiones.

## Ver También

Para más información y ejemplos, puedes visitar las siguientes páginas:

- Documentación del paquete `time`: https://hackage.haskell.org/package/time
- Tutorial sobre fechas y horas en Haskell: https://www.schoolofhaskell.com/user/utdemir/haskell-getting-current-time
- GHC, el compilador de Haskell que usamos en este artículo: https://www.haskell.org/ghc/
- Documentación del módulo `Data.Time`: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html