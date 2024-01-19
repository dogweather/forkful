---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Obtener la longitud de una cadena significa simplemente contar cuántos caracteres hay en una cadena. Los programadores lo hacen para llevar a cabo diversas acciones, como la iteración a través de caracteres o la validación de entradas.

## Cómo se hace:

A continuación, se muestra cómo encontrar la longitud de una cadena en Haskell:

```Haskell
longitudCadena :: String -> Int
longitudCadena = length
```

En el entorno ghci, puedes probarlo de la siguiente manera:

```Haskell
ghci> longitudCadena "Hola, Mundo"
11
```

## Deep Dive

1. **Contexto histórico**: La función 'length' en Haskell ha existido desde la primera versión de Haskell, Haskell 1.0, lanzada en 1990.
2. **Alternativas**: Si prefieres no utilizar la función incorporada, puedes usar una función recursiva para encontrar la longitud de una cadena. Por ejemplo:

```Haskell
longitudCadena' :: String -> Int
longitudCadena' [] = 0
longitudCadena' (_:xs) = 1 + longitudCadena' xs
```

Esta versión de la función funciona al hacer coincidir el patrón con una cadena. Si la cadena está vacía (`[]`), devolverá 0. Si no lo está, eliminará el primer caracter (significado por `_`) y se llamará a sí misma en la cadena restante (`xs`), sumando 1 cada vez.

3. **Detalles de implementación**: La función 'length' en Haskell es muy eficiente, ya que opera en tiempo lineal en relación al tamaño del input. Su implementación básica es bastante similar a la versión recursiva que mostramos como alternativa. 

## Ver También:

Haskell Wiki: [Working with lists](https://wiki.haskell.org/Working_with_lists)

Learn You a Haskell: [Strings](http://learnyouahaskell.com/starting-out#strings)

Haskell Documentation: [Data.List](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)