---
title:                "Haskell: Capitalizar una cadena"
simple_title:         "Capitalizar una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena de texto en Haskell?

A menudo, al trabajar con lenguajes de programación, nos encontramos con la necesidad de modificar un string para que tenga un aspecto específico. En el caso de Haskell, la capitalización de una cadena es una práctica común para mejorar la presentación de datos y facilitar su lectura.

## Cómo hacerlo en Haskell

Para capitalizar una cadena en Haskell, podemos utilizar la función `toUpper` del módulo `Data.Char`. Esta función recibe como parámetro un carácter y devuelve su versión en mayúscula. Podemos aplicar `toUpper` a cada uno de los caracteres de la cadena utilizando la función `map`.

```Haskell
import Data.Char

-- función que capitaliza una cadena
capitalizeString :: String -> String
capitalizeString = map toUpper
```

Veamos ahora un ejemplo de cómo se utiliza esta función:

```Haskell
ghci> capitalizeString "hola mundo"
"HOLA MUNDO"
```

Como podemos ver, nuestra función `capitalizeString` toma una cadena en minúsculas y devuelve una cadena en mayúsculas. También podemos aplicar esta función a cadenas que contengan caracteres especiales:

```Haskell
ghci> capitalizeString "¡hola mundo!"
"¡HOLA MUNDO!"
```

## Profundizando en la capitalización de cadenas en Haskell

La función `capitalizeString` que hemos creado sólo funciona para cadenas que contienen caracteres ASCII. Sin embargo, Haskell nos ofrece herramientas para manejar cadenas con caracteres Unicode, como la función `toUpper` del módulo `Data.Text`, que soporta caracteres de diferentes lenguajes.

También es importante mencionar que, al utilizar la función `map` para aplicar `toUpper` a cada carácter de una cadena, estamos generando una nueva cadena en mayúsculas y dejando la original intacta en memoria. En cambio, si utilizamos la función `map` del módulo `Data.Text`, que funciona con estructuras más eficientes, podemos modificar la cadena original sin necesidad de crear una nueva.

## Ver también

- [Documentación del módulo `Data.Char` en Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Documentación del módulo `Data.Text` en Haskell](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)