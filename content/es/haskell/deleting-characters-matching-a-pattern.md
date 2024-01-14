---
title:    "Haskell: Eliminando caracteres que coinciden con un patrón"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué
El borrado de caracteres que coinciden con un patrón puede ser útil para limpiar datos o realizar cambios en un archivo de texto. Esto puede ahorrar tiempo y facilitar la manipulación de grandes cantidades de datos de forma eficiente.

## Cómo hacerlo
Para este ejemplo, utilizaremos el lenguaje de programación Haskell, conocido por su capacidad de procesar y manipular datos de manera concisa y elegante. Usaremos la función "filter" para encontrar y eliminar los caracteres que coincidan con nuestro patrón.

```Haskell
-- Definir el patrón que queremos encontrar
pattern :: Char -> Bool
pattern c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'

-- Aplicar la función filter al texto de entrada
filteredText = filter (not . pattern) "Este es un ejemplo de texto con varias vocales a eliminar"

-- Imprimir el resultado
print filteredText
-- Salida: "st s n jmpl d txtn cn vrs vcls lmnrr"
```

En este ejemplo, definimos una función "pattern" que determina si un carácter es una vocal. Luego, aplicamos la función "filter" a nuestro texto de entrada, especificando que queremos eliminar los caracteres que coinciden con nuestro patrón. El resultado es una cadena de texto con todas las vocales eliminadas.

## Profundizando
La función "filter" en Haskell toma una función booleana como primer argumento y una lista como segundo argumento. Retorna una nueva lista que contiene los elementos de la lista original que cumplen con la función booleana.

En el ejemplo anterior, utilizamos la función "not" para negar el resultado de nuestra función "pattern". Esto significa que nuestra función "filter" eliminará todos los caracteres que coinciden con nuestro patrón, en lugar de mantenerlos.

## Ver también
- [Documentación de la función filter en Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:filter)
- [Cómo trabajar con textos en Haskell](https://haskell-for-readers.nomeata.de/text-processing.html)