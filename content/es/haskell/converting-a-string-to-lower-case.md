---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertir un String a lowercase en Haskell

## ¿Qué & Por qué?

Convertir un string a lowercase es el proceso de cambiar todas las letras mayúsculas en una cadena de caracteres a letras minúsculas. Los programadores a menudo lo hacen para normalizar los datos de entrada y hacer comparaciones sensibles a las mayúsculas y minúsculas.

## Cómo hacerlo:

En Haskell, podemos usar la función `map` en combinación con la función predefinida `toLower` de la biblioteca `Data.Char`:

```haskell
import Data.Char (toLower)

toLowerString :: String -> String
toLowerString = map toLower
```
Por ejemplo:
```haskell
main :: IO ()
main = do
    print $ toLowerString "HELLO, WORLD!"  -- Output: "hello, world!"
```

## Profundización 

1. **Contexto histórico:** Haskell es un lenguaje puro funcional que se desarrolló a finales de los años 80 y principios de los 90. La facilidad de uso de las funciones map y toLower para modificar contenido string es un buen ejemplo de su enfoque funcional.
2. **Alternativas:** En Haskell, el uso de map y toLower es la forma más común de convertir un string a lowercase. Sin embargo, a veces puedes encontrarte con bibliotecas de terceros que implementan esta funcionalidad en formas más optimizadas.
3. **Detalles de la implementación:** La función `toLower` se implementa en Haskell utilizando Unicode. Como resultado, esta función puede manejar incluso los caracteres más allá de la ASCII básica de 128 caracteres, siempre que obedezcan la norma de mayúsculas/minúsculas Unicode.

## Ver también

Aquí tienes algunos enlaces útiles para obtener más información:

1. [Haskell Wiki](https://wiki.haskell.org)
2. [Data.Char toLower](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html#v:toLower)
3. [Haskell Programming from First Principles](http://haskellbook.com/)