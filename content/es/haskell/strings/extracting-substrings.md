---
title:                "Extracción de subcadenas"
aliases:
- es/haskell/extracting-substrings.md
date:                  2024-01-20T17:45:43.423230-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Extraer subcadenas significa tomar trozos específicos de un texto. Los programadores lo hacen para analizar, transformar o simplemente acceder a partes relevantes de los datos.

## Cómo hacerlo:
Aquí tienes ejemplos de cómo extraer subcadenas:

```Haskell
import Data.Text (Text, pack, unpack, take, drop)

-- Crear una función para empaquetar y desempaquetar cadenas
substring :: Int -> Int -> String -> String
substring start end = unpack . take (end - start) . drop start . pack

main :: IO ()
main = do
    let text = "¡Hola, aficionados a Haskell!"
    putStrLn $ substring 7 18 text -- "aficionados"
```

Salida de la muestra: `"aficionados"`

## Exploración Profunda
Extraer subcadenas es un concepto antiguo, tan viejo como los propios lenguajes de programación. En Haskell, se pueden usar las funciones `take` y `drop` del módulo `Data.Text` para hacer el trabajo, pero hay otras bibliotecas, como `Data.ByteString`, para diferentes tipos de datos.

La elección entre `Data.Text` y `Data.ByteString` depende de si estás trabajando con texto estructurado (como HTML o JSON) o datos binarios. `Data.Text` está optimizado para Unicode, haciéndolo ideal para texto multilingüe.

## Ver También
Para profundizar, aquí hay algunos enlaces:

- Documentación oficial de Haskell: [Haskell Text](https://www.stackage.org/haddock/lts-18.18/text-1.2.4.1/Data-Text.html)
- Tutorial de Haskell sobre `bytestring`: [Haskell ByteString](https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString.html)
