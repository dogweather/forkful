---
title:                "Haskell: Capitalización de una cadena"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en Haskell?

Capitalizar una cadena en Haskell es un proceso bastante común al trabajar con cadenas de texto. La capitalización consiste en convertir la primera letra de cada palabra en mayúscula, lo que puede ser útil para mejorar la legibilidad o para cumplir con ciertos estándares de formato. En esta publicación, aprenderemos cómo capitalizar una cadena en Haskell y profundizaremos en los conceptos detrás de este proceso.

## Cómo hacerlo

Hay varias formas de capitalizar una cadena en Haskell, pero una de las más comunes es utilizando la función `toUpper` del módulo `Data.Char`. Esta función toma un carácter como entrada y devuelve el mismo carácter en mayúscula.

```Haskell
import Data.Char (toUpper)

toUpper 'a' -- devuelve 'A'
```

Para capitalizar una cadena completa, podemos utilizar la función `map` para aplicar la función `toUpper` a cada carácter de la cadena.

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str

capitalize "hola mundo" -- devuelve "HOLA MUNDO"
```

Otra opción es utilizar la función `unwords` junto con `map` para capitalizar cada palabra en una cadena.

```haskell
import Data.Char (toUpper)

capitalizeWords :: String -> String
capitalizeWords str = unwords $ map capitalize $ words str

capitalizeWords "hola mundo" -- devuelve "Hola Mundo"
```

## Profundizando en la capitalización de cadenas

Como podemos ver en los ejemplos anteriores, capitalizar una cadena en Haskell es una tarea relativamente sencilla. Sin embargo, es importante tener en cuenta que la función `toUpper` solo funciona para caracteres individuales y no para cadenas completas.

Por lo tanto, al aplicarla a una cadena, primero debemos separarla en una lista de caracteres y luego volver a unirlos después de aplicar la función `toUpper`. Además, debemos tener en cuenta la codificación de caracteres utilizada en nuestra cadena, ya que puede afectar el resultado de la función `toUpper`.

También podemos aprovechar las ventajas de la programación funcional para crear una función más general que pueda capitalizar cualquier letra en una cadena. Por ejemplo, podemos utilizar la función `zipWith` para combinar dos listas y aplicar una función a cada par de elementos.

```haskell
import Data.Char (isLower, toUpper)

capitalize :: String -> String
capitalize str = zipWith toCapitalize str [0..]
    where toCapitalize char index = if index == 0 || (isLower char) then toUpper char else char

capitalize "hola mundo" -- devuelve "Hola Mundo"
```

En este ejemplo, utilizamos la función `isLower` para determinar si un carácter es una letra minúscula y la condición del `if` para asegurarnos de que solo se capitalice la primera letra de cada palabra.

## Ver también

- [Funciones de caracteres en Haskell](https://www.haskell.org/tutorial/characters.html)
- [Funciones de listas en Haskell](https://www.haskell.org/tutorial/characters.html)
- [Aprenda Haskell en 10 minutos](https://wiki.haskell.org/Learn_Haskell_in_10_minutes)