---
date: 2024-01-20 17:42:17.063324-07:00
description: "C\xF3mo hacerlo: La eliminaci\xF3n de caracteres por coincidencia de\
  \ patrones no es algo nuevo. Desde los primeros d\xEDas de Unix, herramientas como\
  \ `sed` o\u2026"
lastmod: '2024-04-05T21:54:00.448004-06:00'
model: gpt-4-1106-preview
summary: "La eliminaci\xF3n de caracteres por coincidencia de patrones no es algo\
  \ nuevo."
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo hacerlo:
```Haskell
import Data.Char (isSpace)
import Text.Regex (mkRegex, subRegex)

-- Elimina todos los dígitos de una cadena
eliminarDigitos :: String -> String
eliminarDigitos = filter (not . isDigit)

-- Usa expresiones regulares para eliminar cualquier "a" o "e"
eliminarAe :: String -> String
eliminarAe = subRegex (mkRegex "[ae]") "_"

-- Ejemplos
main :: IO ()
main = do
    putStrLn $ eliminarDigitos "H4sk3ll 3s f4nt4st1c0"
    putStrLn $ eliminarAe "Haskell es fantastico"

-- Salida:
-- Hskll s fntstc
-- H_sk_ll _s f_nt_stico
```

## Profundización
La eliminación de caracteres por coincidencia de patrones no es algo nuevo. Desde los primeros días de Unix, herramientas como `sed` o lenguajes como Perl se especializaban en este tipo de tareas gracias a las expresiones regulares. En Haskell, utilizamos bibliotecas como `Text.Regex` para manipulaciones de texto robustas y expresivas. 

Las alternativas en Haskell incluyen usar funciones como `filter` para casos sencillos o incluso escribir parsers más complejos con herramientas como `parsec` o `megaparsec` cuando las necesidades son más específicas. 

La implementación puede variar desde funciones puras hasta monadas de entrada/salida (IO) dependiendo de si los datos se procesan en tiempo de ejecución o se leen desde un archivo, por ejemplo.

## Ver También
- Paquete `regex-base`: [Hackage - regex-base](https://hackage.haskell.org/package/regex-base)
- Paquete `regex-posix`, que permite expresiones regulares POSIX: [Hackage - regex-posix](https://hackage.haskell.org/package/regex-posix)
- Documentación de Haskell sobre listas y funciones como `filter`: [Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#texas-ranges)
- Una introducción más exhaustiva a `parsec`: [Hackage - parsec](https://hackage.haskell.org/package/parsec)
