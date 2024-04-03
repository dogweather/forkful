---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:30.973636-07:00
description: "Recuperar la fecha actual en Haskell implica obtener el tiempo actual\
  \ del sistema y transformarlo en un formato de fecha legible. Los programadores\
  \ hacen\u2026"
lastmod: '2024-03-13T22:44:59.128954-06:00'
model: gpt-4-0125-preview
summary: Recuperar la fecha actual en Haskell implica obtener el tiempo actual del
  sistema y transformarlo en un formato de fecha legible.
title: Obteniendo la fecha actual
weight: 29
---

## ¿Qué y Por Qué?
Recuperar la fecha actual en Haskell implica obtener el tiempo actual del sistema y transformarlo en un formato de fecha legible. Los programadores hacen esto para realizar operaciones basadas en la fecha, como registro de actividades, programación de tareas o estampado de tiempo en eventos en aplicaciones.

## Cómo hacerlo:
La biblioteca estándar de Haskell, `base`, proporciona el módulo `Data.Time` que ofrece funcionalidad para trabajar con fechas y horas. He aquí cómo usarlo para obtener la fecha actual:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

Ejemplo de salida:
```
2023-04-12
```

Para mayor flexibilidad, como formatear la fecha o trabajar con diferentes zonas horarias, la biblioteca `time` es invaluable. Así es cómo podrías formatear la fecha actual:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

Esto imprime la fecha actual en el formato `AAAA-MM-DD`, ajustada a la zona horaria local.

Adicionalmente, para el soporte de bibliotecas de terceros, `time` es altamente recomendado y a menudo utilizado dentro de la comunidad de Haskell por sus extensas capacidades de manipulación de fechas y horas. Los ejemplos anteriores utilizan esta biblioteca.

Si necesitas una manipulación de fechas más completa, incluyendo el parseo desde cadenas o operaciones aritméticas con fechas y horas, explorar funciones adicionales dentro de `Data.Time` será beneficioso.
