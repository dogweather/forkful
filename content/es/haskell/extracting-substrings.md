---
title:                "Extrayendo subcadenas"
html_title:           "Haskell: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

¿Por qué extraer subcadenas en Haskell?

Extraer subcadenas, o secciones de una cadena, es una tarea común en muchos programas. En Haskell, esto se logra utilizando la función `take` y `drop` para obtener una subcadena a partir de una posición inicial y final. Esto es especialmente útil cuando se trabaja con grandes cantidades de datos o algoritmos de procesamiento de texto.

## Cómo hacerlo

Para extraer una subcadena en Haskell, simplemente se utiliza la función `take` y `drop` con la cadena original y los índices específicos. Por ejemplo, si queremos obtener la subcadena "ello" de la cadena "Hola mundo", podemos escribir lo siguiente en el intérprete GHCi:

```
Haskell
take 4 (drop 1 "Hola mundo")
```

Esto nos dará como resultado la subcadena deseada. También podemos utilizar variables en lugar de valores fijos para especificar los índices. Por ejemplo:

```
Haskell
let cadena = "Hola mundo"
let inicio = 1
let fin = 4
take fin (drop inicio cadena)
```

En este caso, la subcadena "Hola" será extraída de la cadena original. Además, Haskell también nos permite utilizar listas para especificar los índices de forma más dinámica. Por ejemplo, si tenemos una lista `indices = [1,3,5]`, podemos usarla para obtener la subcadena "olm" de la cadena "Hola mundo" de la siguiente manera:

```
Haskell
let cadena = "Hola mundo"
let indices = [1,3,5]
map (cadena !!) indices
```

Este último ejemplo utiliza la función `map` para aplicar la función `!!` sobre cada uno de los índices especificados en la lista, obteniendo así los caracteres correspondientes y formando la subcadena deseada.

## Profundizando

Además de las funciones `take` y `drop`, Haskell también cuenta con otras funciones útiles para trabajar con subcadenas. Entre ellas se encuentran `splitAt` para dividir una cadena en dos partes y `takeWhile` y `dropWhile` para extraer subcadenas basadas en una condición determinada.

También es posible utilizar patrones en la definición de funciones para extraer subcadenas específicas. Por ejemplo, podemos definir una función `primerosTres` que devuelve siempre los primeros tres caracteres de una cadena dada:

```
Haskell
primerosTres (x:y:z:_) = [x,y,z]
```

Así, podemos obtener los primeros tres caracteres de cualquier cadena simplemente llamando a esta función con la cadena como argumento.

## Ver también

1. Documentación oficial de Haskell sobre `take` y `drop`: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:take
2. Tutorial de Learn You a Haskell para obtener subcadenas: https://learnyouahaskell.com/starting-out#bringing-functions-into-scope
3. Ejemplos prácticos de uso de funciones de subcadenas en Haskell: https://stackoverflow.com/questions/28925927/string-slicing-in-haskell