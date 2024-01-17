---
title:                "Uniendo cadenas"
html_title:           "Haskell: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?:
La concatenación de cadenas se refiere a la acción de unir dos o más cadenas de texto juntas en una sola cadena. Los programadores suelen hacer esto para combinar información y crear nuevas cadenas que puedan ser utilizadas en su código.

## Cómo hacerlo:
El lenguaje de programación Haskell proporciona varias formas de concatenar cadenas. Una forma es utilizando el operador de concatenación `++`, que une dos cadenas juntas. Por ejemplo: 

```Haskell
"¡Hola! " ++ "¿Cómo estás?"  
```
Este código producirá la salida: `¡Hola! ¿Cómo estás?` 

Otra forma de concatenar cadenas es utilizando la función `concat`, que puede combinar múltiples cadenas en una sola. Por ejemplo:

```Haskell
concat ["¡Hola", " ", "mundo", "!"]
```

Este código producirá la salida: `¡Hola mundo!`

## Deep Dive:
La concatenación de cadenas ha sido utilizada en la programación desde los primeros días de los lenguajes de programación. En Haskell, el operador `++` fue agregado por primera vez en la versión 1.2 en 1991. 

Además de utilizar el operador `++` y la función `concat`, también existen otras formas de concatenar cadenas en Haskell, como por ejemplo utilizando la función `foldl` o utilizando listas de carácteres.

## Ver también:
- [Documentación oficial de Haskell](https://www.haskell.org/)
- [Tutorial de Haskell en español](https://wiki.haskell.org/Espa%C3%B1ol/Tutorial_de_Haskell)