---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra a mayúscula. Los programadores a menudo lo hacen para garantizar que los nombres propios o títulos se muestren correctamente en una interfaz de usuario o en documentos generados por programas.

## Cómo hacerlo:

```Haskell
import Data.Char(toUpper)
import Data.List.Split(splitOn)

capitalize :: String -> String
capitalize [] = []
capitalize str = unwords $ map (\(x:xs) -> toUpper x : xs) $ words $ lowerString str

lowerString :: String -> String
lowerString = map toLower

-- Usando la función capitalize
main :: IO ()
main = putStrLn $ capitalize "hola, este es un ejemplo en haskell."

-- Salida: "Hola, Este Es Un Ejemplo En Haskell."
```

## Análisis Profundo:

Capitalizar cadenas no es nuevo; es un concepto ya presente en los días de las máquinas de escribir para enfatizar títulos o nombres. En Haskell, se puede hacer funcionalmente con funciones como `map` y `toUpper`. Una alternativa es usar bibliotecas como `Data.Text`, que tiene funciones incorporadas para esto. La implementación manual implica convertir cada palabra a su forma en minúsculas y luego capitalizar la primera letra, asegurando la uniformidad independientemente de la entrada original.

## Ver También:

- Haskell Documentation for `Data.Char`: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- Haskell Documentation for `Data.List.Split`: https://hackage.haskell.org/package/split-0.2.3.4/docs/Data-List-Split.html
- StackOverflow para preguntas prácticas: https://stackoverflow.com/questions/tagged/haskell
