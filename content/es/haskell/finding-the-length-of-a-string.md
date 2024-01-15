---
title:                "Encontrando la longitud de una cadena"
html_title:           "Haskell: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

El encontrar la longitud de una cadena de caracteres es un desafío común y útil para cualquier programador que trabaje con texto. Puede ser necesario para tareas como la validación de entradas de usuario o para el procesamiento de datos en un programa.

## Cómo

En Haskell, la longitud de una cadena se puede encontrar utilizando la función `length`. Esta función toma una cadena como argumento y devuelve un número entero que representa la cantidad de caracteres en la cadena.

Por ejemplo:

```Haskell
main = do
    let cadena = "Hola, mundo!"
    print (length cadena)
```

El resultado sería:

```
13
```

## Profundizando

La función `length` en Haskell es en realidad una instancia de la clase de tipos `Foldable`. Esto significa que no solo se puede utilizar con cadenas, sino también con otros tipos de datos como listas, árboles o conjuntos.

Además, la función `length` no solo cuenta la cantidad de caracteres en una cadena, sino que también puede utilizar cualquier tipo de "contenedor" que definamos para devolver la cantidad de elementos que contiene.

Por ejemplo, podríamos crear una función `miLongitud` que cuenta la cantidad de elementos en una lista:

```Haskell
miLongitud :: [a] -> Int
miLongitud = foldr (\_ y -> y + 1) 0
```

Luego, podemos utilizar esta función con cualquier lista, en lugar de solo cadenas:

```Haskell
main = do
    let lista = [1,2,3,4,5]
    print (miLongitud lista)
```

El resultado sería:

```
5
```

## Ver también

- [Módulo de Strings en Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html)
- [Función `length` en Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:length)