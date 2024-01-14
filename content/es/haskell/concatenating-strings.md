---
title:                "Haskell: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar cadenas de texto es una operación común en programación. Puede ser útil cuando se desea combinar múltiples cadenas en una sola, ya sea para su impresión o manipulación posterior.

## Cómo hacerlo

En Haskell, se puede utilizar la función `++` para concatenar dos cadenas de texto. Por ejemplo:

```Haskell
"¡Hola " ++ "mundo!"
```

El resultado de este código sería "¡Hola mundo!".

También se puede concatenar más de dos cadenas utilizando el mismo operador. Por ejemplo:

```Haskell
"La " ++ "vida " ++ "es " ++ "una " ++ "carnaval"
```

Esto produciría "La vida es una carnaval".

Si se quisiera añadir un espacio entre las cadenas, se puede utilizar la función `concat` en conjunto con la lista de cadenas:

```Haskell
concat ["Hoy", " ", "es", " ", "un", " ", "buen", " ", "día"]
```

Esto daría como resultado "Hoy es un buen día".

## Profundizando

La concatenación de strings en Haskell se basa en dos conceptos: la evaluación perezosa y los tipos de datos inmutables. Esto significa que la concatenación de cadenas no cambia los valores originales, sino que crea una nueva cadena como resultado.

También es importante tener en cuenta que la concatenación de cadenas puede ser costosa en términos de rendimiento, especialmente cuando se están concatenando muchas cadenas. En ese caso, se puede utilizar la función `concat` para mejorar la eficiencia.

## Ver también

- [Documentación de la función `++`](https://hackage.haskell.org/package/base-4.15.1.0/docs/Prelude.html#v:-43--43-)
- [Documentación de la función `concat`](https://hackage.haskell.org/package/base-4.15.1.0/docs/Prelude.html#v:concat)