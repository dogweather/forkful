---
title:                "Uso de expresiones regulares"
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Las expresiones regulares son patrones utilizados para coincidir con combinaciones de caracteres en cadenas de texto. Los programadores las usan para buscar, editar o validar datos de manera rápida y eficiente.

## Cómo hacerlo:

Primero, instala el paquete `regex-posix` si aún no lo tienes:

```shell
cabal update
cabal install regex-posix
```

Ahora, puedes usar expresiones regulares en Haskell. Aquí unos ejemplos:

```haskell
import Text.Regex.Posix

-- Verifica si un texto coincide con un patrón
"hello world" =~ "world" :: Bool
-- Salida: True

-- Busca y extrae la primera coincidencia
"busco un número 123" =~ "[0-9]+" :: String
-- Salida: "123"

-- Encuentra todas las coincidencias
"los números son 123 y 456" =~ "[0-9]+" :: [String]
-- Salida: ["123", "456"]
```

## Profundización

Las expresiones regulares tienen sus raíces en la teoría de autómatas y lenguajes formales, siendo popularizadas en las décadas de 1960 y 1970 con herramientas como `grep` en UNIX. Alternativas en Haskell incluyen los paquetes `regex-pcre` y `regex-tdfa`, cada uno con diferentes capacidades y rendimiento. Internamente, las expresiones regulares pueden implementarse con algoritmos de backtrack o a través de construcción y simulación de autómatas finitos.

## Ver También

- [Hackage: regex-posix](https://hackage.haskell.org/package/regex-posix)
- [Haskell Wiki: Regular Expressions](https://wiki.haskell.org/Regular_expressions)
- [Learn You a Haskell for Great Good! - Using regex with Haskell](http://learnyouahaskell.com/input-and-output#files-and-streams)
