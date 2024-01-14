---
title:                "Haskell: Borrando caracteres que coincidan con un patrón"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, al trabajar con datos y cadenas de texto en Haskell, es necesario limpiar y formatear los mismos para poder realizar operaciones o análisis específicos. Una forma de hacer esto es eliminando ciertos caracteres que no sean necesarios o que no cumplan con un patrón específico. En esta entrada del blog, aprenderemos cómo eliminar caracteres que coincidan con un patrón en Haskell, y por qué podría ser útil hacerlo.

## Cómo hacerlo

Una forma de eliminar caracteres que coincidan con un patrón en Haskell es utilizando la función `deleteBy` de la biblioteca `Data.List`. Esta función toma dos argumentos: una función que determina si un elemento coincide con el patrón, y una lista de elementos en la que se realizará la eliminación. Por ejemplo, si queremos eliminar todas las letras mayúsculas de una cadena de texto, podemos hacerlo de la siguiente manera:

```Haskell
import Data.Char (isUpper)

deleteBy (==) "Haskell" -- devolverá "askell"
```

En este ejemplo, la función `isUpper` se utiliza como la función de comparación, ya que devuelve `True` si un caracter es una letra mayúscula. La función `deleteBy` entonces eliminará todos los caracteres que cumplan con este patrón, dejando solo las letras minúsculas en la cadena de texto.

Otra forma de eliminar caracteres que coincidan con un patrón es utilizando la función `filter` de la biblioteca `Data.List`. Esta función también toma una función como argumento, pero en este caso, la función debe devolver `True` para mantener el elemento en la lista, y `False` para eliminarlo. Por ejemplo, si queremos eliminar todos los números de una lista de enteros, podemos hacerlo de la siguiente manera:

```Haskell
import Data.Char (isDigit)

filter (\x -> not (isDigit x)) [1, 2, 3, 4, 5, 6, 7, 8, 9] -- devolverá una lista vacía
```

En este ejemplo, se utiliza la función `isDigit` para determinar si un número es un dígito. La función `filter` luego elimina todos los elementos que cumplan con este patrón, dejando una lista vacía como resultado.

## Deep Dive

La función `deleteBy` también es capaz de manejar patrones más complejos utilizando funciones de comparación personalizadas. Esto se puede hacer proporcionando una función que tome dos argumentos (los dos elementos que se están comparando) y devuelve un tipo de ordenamiento (`LT`, `EQ` o `GT`). Por ejemplo, si queremos eliminar todos los números pares de una lista de enteros, podemos hacerlo de la siguiente manera:

```Haskell
deleteBy compareNumbers [1, 2, 3, 4, 5, 6, 7, 8, 9] 
    where compareNumbers x y 
              | odd x && even y = LT
              | odd x && odd y = EQ
              | otherwise = GT

-- devolverá [1, 3, 5, 7, 9]
```

En este ejemplo, la función `compareNumbers` primero verifica si `x` es impar y `y` es par. Si esto es cierto, se devuelve `LT`, lo que significa que `x` debe eliminarse de la lista. Si ambos `x` e `y` son impares, se devuelve `EQ`, lo que significa que `x` se mantendrá en la lista. En todos los demás casos, se devuelve `GT`, lo que significa que tanto `x` como `y` se mantendrán en la lista.

## Véase también

- [Documentación de `Data.List.deleteBy`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:deleteBy)
- [Documentación de `Data.List.filter`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:filter)