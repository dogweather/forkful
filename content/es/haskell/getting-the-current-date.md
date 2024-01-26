---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:14:56.942143-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en Haskell es simplemente capturar la fecha y hora en este preciso momento. Los programadores hacen esto para registrar eventos, manejar operaciones relacionadas con el tiempo o simplemente mostrar la fecha al usuario.

## Cómo hacerlo:
Primero, necesitas importar el módulo `Data.Time`. Aquí hay dos funciones principales para obtener la fecha: `getCurrentTime`, que te da la hora y fecha UTC, y `getCurrentTimeZone` junto con `utcToLocalTime`, que convierten la hora UTC a tu zona horaria local.

```Haskell
import Data.Time

main :: IO ()
main = do
    -- Obtener la hora y fecha UTC actual
    utcTime <- getCurrentTime
    print utcTime

    -- Convertir a hora y fecha local
    zone <- getCurrentTimeZone
    let localTime = utcToLocalTime zone utcTime
    print localTime
```
Salida de muestra:
```
2023-03-28 12:45:23.651971 UTC
2023-03-28 14:45:23.651971 CET
```
Cambia los segundos según el momento en que ejecutes el código. La función `print` mostrará la hora en un formato estándar.

## Análisis Profundo:
Haskell ha tenido soporte para manejar fechas y horas desde sus primeras versiones. El paquete `time` es el standard en Haskell para trabajar con tiempo. Algunas alternativas a `Data.Time` incluyen `old-time` y librerías de terceros como `thyme`.

`getCurrentTime` te da un valor de tipo `UTCTime`, que es básicamente un punto en tiempo universal coordinado (UTC). Usar UTC ayuda a evitar los líos que pueden surgir con zonas horarias y cambios de hora.

Si necesitas manejar fechas de manera más compleja, hay funciones como `addUTCTime` para sumar segundos, o `diffUTCTime` para obtener la diferencia entre dos momentos. Cada una manipula la fecha y hora de manera precisa y son parte de `Data.Time`.

## Consultas Relacionadas:
- Documentación de `Data.Time`: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Tutorial de Haskell: http://learnyouahaskell.com/
- Información sobre el manejo de zonas horarias: https://www.iana.org/time-zones

Recuerda que las prácticas de manejo de tiempo están en constante evolución, así que mantenerse al día es clave para un manejo efectivo de fechas y horas en Haskell.
