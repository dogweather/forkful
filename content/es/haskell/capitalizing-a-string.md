---
title:                "Majusculeando una cadena"
html_title:           "Haskell: Majusculeando una cadena"
simple_title:         "Majusculeando una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Capitalizar una cadena de texto significa convertir la primera letra de cada palabra en mayúscula y el resto en minúscula. Los programadores lo hacen para mejorar la legibilidad y presentación de sus códigos, ya que facilita distinguir entre palabras individuales y resaltar nombres de variables o funciones.

## Cómo:
```Haskell
capitalize :: String -> String
capitalize str = unwords $ map (\x -> toUpper (head x) : tail x) $ words str

main = do
    putStrLn $ capitalize "hola mundo" -- Hola Mundo
    putStrLn $ capitalize "myNameIsJohn" -- Mynameisjohn
```

## Profundizando:
La capitalización de cadenas de texto se basa en la escritura en mayúscula y minúscula que se utiliza en inglés. Otras alternativas para mejorar la legibilidad incluyen el uso de mayúsculas y minúsculas en negrita o cursiva. La implementación en Haskell se logra mediante el uso de funciones como `map` y `toUpper` que permiten iterar sobre una lista de caracteres y convertirlos a su equivalente en mayúscula.

## Ver también:
- [Un tutorial sobre cómo capitalizar cadenas en Haskell](https://dev.to/king_11/capitalizing-strings-in-haskell-4h21)
- [Documentación oficial de funciones en Haskell](https://www.haskell.org/documentation/#string-manipulation)