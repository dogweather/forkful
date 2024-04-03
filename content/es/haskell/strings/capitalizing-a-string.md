---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:18.589695-07:00
description: "C\xF3mo hacerlo: En Haskell, puedes capitalizar una cadena usando la\
  \ biblioteca est\xE1ndar sin necesidad de bibliotecas de terceros."
lastmod: '2024-03-13T22:44:59.102983-06:00'
model: gpt-4-0125-preview
summary: "En Haskell, puedes capitalizar una cadena usando la biblioteca est\xE1ndar\
  \ sin necesidad de bibliotecas de terceros."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
En Haskell, puedes capitalizar una cadena usando la biblioteca estándar sin necesidad de bibliotecas de terceros.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Uso de muestra:
main = putStrLn $ capitalize "hello world"
```

Salida:
```
Hello world
```

Para escenarios más complejos o facilidad de uso, es posible que quieras usar una biblioteca de terceros como `text`, que es popular para la manipulación eficiente de cadenas en Haskell.

Primero, necesitas agregar `text` a las dependencias de tu proyecto. Luego, puedes usar sus funciones para capitalizar una cadena de la siguiente manera:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Uso de muestra con la biblioteca text:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Salida:
```
Hello world
```

Ambos ejemplos demuestran maneras simples pero efectivas de capitalizar una cadena en Haskell, con o sin bibliotecas de terceros.
