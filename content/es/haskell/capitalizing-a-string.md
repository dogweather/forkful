---
title:                "Mayúsculas en una cadena"
html_title:           "Haskell: Mayúsculas en una cadena"
simple_title:         "Mayúsculas en una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Uno puede querer capitalizar una cadena de texto para hacer que la primera letra de cada palabra sea mayúscula. Esto puede ser útil en situaciones como imprimir nombres propios o títulos.

## Cómo hacerlo

La forma más sencilla de capitalizar una cadena en Haskell es utilizando la función `toUpper` del módulo `Data.Char`. Tomemos como ejemplo la cadena "hola mundo" y veamos cómo capitalizarla:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = (toUpper (head str)) ++ (tail str)

main = putStrLn (capitalize "hola mundo")
```

Este código imprimirá "Hola mundo" en la consola. Aquí explicamos lo que hace cada línea:

- La primera línea importa la función `toUpper` del módulo `Data.Char`, que nos permite convertir un carácter en su versión mayúscula.
- La segunda línea define una función llamada `capitalize` que toma una cadena y devuelve otra cadena. El parámetro `str` representa la cadena que queremos capitalizar.
- En la tercera línea, usamos la función `toUpper` para convertir la primera letra de `str` en mayúscula. Luego, con el operador `++`, concatenamos esa letra con el resto de la cadena, que se obtiene con la función `tail` que devuelve todos los elementos excepto el primero de una lista.
- La última línea simplemente imprime el resultado de llamar a la función `capitalize` con la cadena "hola mundo" como argumento.

## Profundizando

Además de la función `toUpper`, el módulo `Data.Char` también contiene otras funciones útiles para trabajar con cadenas de caracteres, como `toLower` que convierte un carácter en su versión minúscula y `isUpper` que verifica si un carácter es mayúscula.

También es importante mencionar que la función `capitalize` que definimos anteriormente solo funciona para cadenas de una sola palabra. Si queremos capitalizar una cadena con varias palabras, podemos usar la función `words` del módulo `Data.Strings`, que convierte una cadena en una lista de palabras, y luego aplicar nuestra función `capitalize` a cada palabra de esa lista.

## Ver también

- [Documentación de Haskell](https://www.haskell.org/documentation/)
- [Curso interactivo de Haskell](https://www.learnhaskell.org/)
- [Introducción a la programación funcional con Haskell](https://www.freecodecamp.org/news/an-introduction-to-functional-programming-with-haskell-12e54e79b6b5/)