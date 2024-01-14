---
title:    "Haskell: Convirtiendo una cadena a minúsculas"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas puede parecer algo trivial, pero en realidad es una tarea útil y necesaria al trabajar con datos de texto en Haskell. Al convertir las letras de una cadena a minúsculas, podemos facilitar la comparación de cadenas y realizar operaciones de búsqueda y ordenamiento con mayor precisión.

## Cómo hacerlo

En Haskell, convertir una cadena a minúsculas es muy sencillo gracias a la función `map`, que nos permite aplicar una operación a cada elemento de una lista. En este caso, utilizaremos la función `toLower` del módulo Data.Char para convertir cada carácter de la cadena a su equivalente en minúscula.

```Haskell
import Data.Char (toLower)

lowercaseString :: String -> String
lowercaseString str = map toLower str

-- Ejemplo de entrada y salida
lowercaseString "HOLA MUNDO" -- "hola mundo"
lowercaseString "Coding in Haskell" -- "coding in haskell"
```

Con la función `lowercaseString` definida, podemos aplicarla a cualquier cadena de texto para obtener su equivalente en minúsculas.

## Profundizando

Al analizar la función `lowercaseString`, podemos ver que se basa en el concepto de `map` y funciones de orden superior, lo que nos permite aplicar transformaciones a los elementos de una lista sin tener que recorrerla manualmente. También es importante mencionar que la función `lowercaseString` es pura, lo que significa que no tiene efectos secundarios y siempre producirá el mismo resultado dado el mismo argumento.

Además de la función `toLower`, el módulo Data.Char también nos ofrece otras funciones útiles para trabajar con caracteres, como `isLower` para determinar si un carácter es una letra minúscula o `isAlpha` para determinar si un carácter es una letra del alfabeto.

## Ver también

- [Documentación de la función `map` en Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:map)
- [Tutorial de funciones de orden superior en Haskell](https://wiki.haskell.org/Higher_order_function)
- [Documentación del módulo Data.Char en Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)