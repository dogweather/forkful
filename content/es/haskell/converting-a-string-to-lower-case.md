---
title:    "Haskell: Convertir una cadena en minúsculas"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, es común encontrarse con la necesidad de manipular strings de texto. Una tarea común es convertir un string a su versión en minúsculas. En este artículo, exploraremos cómo realizar esta tarea en Haskell.

## Cómo hacerlo
Para convertir un string a minúsculas en Haskell, podemos usar la función `toLower` de la librería `Data.Char`. Esta función toma como argumento un `Char` y devuelve el mismo carácter en minúscula.

```Haskell
import Data.Char (toLower)

-- Ejemplo de uso de la función toLower
toLower 'H' -- Devuelve 'h'
toLower 'E' -- Devuelve 'e'
```

Sin embargo, para aplicarlo a un string completo, necesitamos usar la función `map` para aplicar `toLower` a cada carácter del string.

```Haskell
convertirAMin :: String -> String
convertirAMin str = map toLower str

-- Ejemplo de uso de la función convertirAMin
convertirAMin "Haskell" -- Devuelve "haskell"
convertirAMin "Proyecto" -- Devuelve "proyecto"
```

También podemos utilizar la función `map` en conjunto con la función `words` para convertir cada palabra de un string a minúsculas.

```Haskell
convertirPalabrasAMin :: String -> String
convertirPalabrasAMin str = unwords $ map convertirAMin (words str)

-- Ejemplo de uso de la función convertirPalabrasAMin
convertirPalabrasAMin "Hola A Todos" -- Devuelve "hola a todos"
convertirPalabrasAMin "Bienvenidos Al Mundo De Haskell" -- Devuelve "bienvenidos al mundo de haskell"
```

## Profundizando
La función `toLower` realiza la conversión a minúsculas basándose en la tabla de caracteres Unicode. Esto significa que no solo funciona para letras del alfabeto, sino también para caracteres especiales y acentos.

Otra forma de convertir un string a minúsculas en Haskell sería utilizando la función `mapM`, que permite aplicar una acción monádica a cada elemento de una lista. En este caso, la acción monádica sería `putStrLn` para imprimir cada carácter en minúscula.

```Haskell
convertirStringAMin :: String -> IO ()
convertirStringAMin str = mapM (putStrLn . toLower) str

-- Ejemplo de uso de la función convertirStringAMin
convertirStringAMin "Haskell" -- Imprime "haskell"
convertirStringAMin "¡Hola Mundo!" -- Imprime "¡hola mundo!"
```

## Ver también
- [Documentación de la función toLower en Hackage](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toLower)
- [Tutorial de Haskell en español](https://www.haskell-es.org/tutorial/)
- [Ejemplos de código en Haskell en GitHub](https://github.com/Haskell-Es/Ejercicios)