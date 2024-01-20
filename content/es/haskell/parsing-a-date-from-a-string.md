---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Analizar una fecha desde una cadena es el proceso de convertir la representación de texto de una fecha en un valor procesable de fecha y hora. Los programadores lo hacen para manipular, calcular, ordenar y almacenar datos temporales de una manera más eficiente.

## ¿Cómo hacerlo?

Primero, importa el módulo de tiempo:
```Haskell
import Data.Time
```
Puedes analizar una fecha usando la función `parseTimeM` y `defaultTimeLocale`:
```Haskell
fecha :: String -> Either String UTCTime
fecha s = parseTimeM True defaultTimeLocale "%Y-%m-%d" s
```  
Por ejemplo, si tienes la cadena de texto "2022-09-25":
```Haskell
main :: IO ()
main = print $ fecha "2022-09-25"
```
Esto imprimirá `2022-09-25 00:00:00 UTC`.

## Más Información

La necesidad de analizar fechas de cadenas ha existido desde los principios de la programación, cuando los datos de fecha y hora a menudo se guardaban como texto debido a limitaciones de almacenamiento. En Haskell, `parseTimeM` y `defaultTimeLocale` son comúnmente utilizados, pero también existen otras alternativas como `read` y `readMaybe` para estructuras de fecha más simples. Sin embargo, dichas funciones no manejan la localización y el formato de la fecha con tanto detalle como `parseTimeM` y `defaultTimeLocale`.

## Ver También

Para más detalles, puedes consultar estos enlaces:

1. [La Documentación Oficial de Haskell](https://www.haskell.org/)
2. [Información sobre Data.Time](http://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time.html)
3. [Tutorial Básico de Haskell](http://learnyouahaskell.com/starting-out#an-intro-to-functions)
4. [Modulo Time de Haskell en StackOverflow](https://stackoverflow.com/questions/tagged/haskell+time)