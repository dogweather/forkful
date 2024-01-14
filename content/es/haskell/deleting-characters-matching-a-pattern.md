---
title:                "Haskell: Borrando caracteres que coincidan con un patrón"
simple_title:         "Borrando caracteres que coincidan con un patrón"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Como programadores en Haskell, a menudo nos encontramos con el desafío de manipular cadenas de texto y realizar diversas operaciones en ellas. Una de estas operaciones puede ser la eliminación de caracteres que coinciden con un patrón específico. Aunque esto puede no ser una tarea común, puede ser útil en ciertas situaciones, especialmente cuando estamos trabajando con grandes cantidades de datos.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Haskell, podemos utilizar la función `filter` y la función `notElem`. Primero, debemos definir el patrón que queremos que cumplan los caracteres que deseamos eliminar. Por ejemplo, si queremos eliminar las vocales de una cadena, podemos definir el patrón como `["a", "e", "i", "o", "u"]`. Luego, podemos usar la función `filter` para filtrar todos los caracteres que no estén incluidos en nuestro patrón con la función `notElem`.

```Haskell
-- Definimos el patrón de caracteres a eliminar
patron = ["a", "e", "i", "o", "u"]

-- Definimos una función que elimina los caracteres que coinciden con el patrón
eliminarCaracteres :: [Char] -> [Char]
eliminarCaracteres cadena = filter (`notElem` patron) cadena

-- Ejemplo de uso
> eliminarCaracteres "Hola Mundo"
"Hl Mnd"
```

También podemos utilizar la función `delete` del módulo `Data.List` para eliminar caracteres específicos en una cadena. Esta función toma como argumentos un elemento y una lista, y devuelve la lista sin el elemento especificado.

```Haskell
import Data.List (delete)

-- Ejemplo de uso
> delete 'a' "Hola Mundo"
"Hol Mundo"
```

## Profundizando

Si queremos eliminar caracteres específicos basados en un patrón más complejo, podemos utilizar la función `deleteBy` del módulo `Data.List` en lugar de `delete`. Esta función toma como argumentos una función de comparación y una lista, y elimina los elementos de la lista que cumplan con la condición de la función de comparación.

Por ejemplo, si queremos eliminar todos los caracteres que sean números, podemos utilizar la función `isDigit` del módulo `Data.Char` en la función `deleteBy`. Esta función devuelve `True` si el carácter es un número y `False` en caso contrario.

```Haskell
import Data.Char (isDigit)
import Data.List (deleteBy)

-- Definimos una función de comparación que retorna True si el carácter es un número
esNumero :: Char -> Bool
esNumero car = isDigit car

-- Ejemplo de uso
> deleteBy esNumero "Hola 123 Mundo"
"Hola Mundo"
```

## Ver también

- [Documentación de Haskell](https://www.haskell.org/documentation/)
- [Módulo Data.List](https://hackage.haskell.org/package/base/docs/Data-List.html)
- [Módulo Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html)