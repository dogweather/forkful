---
title:                "Haskell: Encontrando la longitud de una cadena"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por Qué

En la programación Haskell, a menudo es necesario encontrar la longitud de una cadena. Esto puede ser útil para una variedad de aplicaciones, desde manipulación de texto hasta análisis de datos. ¡Sigue leyendo para aprender cómo hacerlo!

## Cómo Hacerlo

Para encontrar la longitud de una cadena en Haskell, usaremos la función `length` incorporada. Esta función toma una lista como argumento y devuelve el número de elementos en la lista. Aquí hay un ejemplo de cómo usarlo para encontrar la longitud de una cadena:

```
Haskell
length "hola" -- devuelve 4
```

También podemos usar la función `length` en una lista de caracteres individuales, como se muestra en el siguiente ejemplo:

```
Haskell
length ['h', 'o', 'l', 'a'] -- devuelve 4
```

## Profundizando

La función `length` funciona encontrando el número de veces que el constructor de la lista `Cons (:)` se aplica en la lista. Por ejemplo, si tenemos la lista `[1, 2, 3]`, la función `length` aplicará `:` tres veces para construir la lista. Esta es una forma eficiente de encontrar la longitud de una lista, ya que no tiene que recorrer toda la lista para contar los elementos.

## Ver También

- [Documentación oficial de la función `length`] (https://www.haskell.org/onlinereport/standard-prelude.html#function-length)
- [Tutorial de Haskell en español] (https://haskell-es.com/)
- [Ejemplos de código Haskell en línea] (https://play.haskell.org/)