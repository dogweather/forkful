---
title:                "Obtener la fecha actual"
html_title:           "Haskell: Obtener la fecha actual"
simple_title:         "Obtener la fecha actual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual es una tarea común para los programadores. Permite a las aplicaciones registrar el momento en que se realizan ciertas acciones, como guardar un archivo o hacer una transacción. También puede ser útil para mostrar información actualizada, como la fecha de hoy en una página web.

## Cómo hacerlo:
Para obtener la fecha actual en Haskell, podemos usar la función `getCurrentTime` del módulo `Data.Time`. Esta función devuelve un valor del tipo `UTCTime`, que representa la fecha y hora en formato universal coordinado (UTC). Luego, podemos convertirlo a un formato más legible usando la función `show` y especificando el formato deseado.

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  putStrLn $ "La fecha actual es: " ++ show (utctDay currentTime)
  putStrLn $ "La hora actual es: " ++ show (utctDayTime currentTime)
```

Output:
```
La fecha actual es: 2020-10-22
La hora actual es: 46336.233797s
```

## Profundizando:
Obtener la fecha actual es una tarea que ha evolucionado con el tiempo. Antiguamente, los sistemas operativos proporcionaban funciones específicas para este propósito, pero ahora, con la popularidad de lenguajes como Haskell, muchas bibliotecas ofrecen formas más elegantes de manejar fechas y horas. Algunas alternativas a la función `getCurrentTime` son `System.Posix.Time.clock_gettime` y `System.Posix.Files.modificationTime`.

## Ver también:
Si estás buscando más información sobre fechas y horas en Haskell, puedes revisar la documentación de los módulos `Data.Time` y `System.Posix.Time`. También puedes explorar otras bibliotecas como `time`, `time-locale-compat`, y `date`.