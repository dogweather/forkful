---
title:    "Haskell: Encontrando la longitud de una cadena"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

En la programación, es importante poder manipular cadenas de texto para realizar diversas tareas. Una de las tareas más comunes es encontrar la longitud de una cadena de texto. En este artículo te mostraremos cómo hacerlo utilizando Haskell.

## Cómo hacerlo

Usando Haskell, podemos encontrar la longitud de una cadena de texto de manera sencilla. Primero, importemos el módulo `Data.List` para poder usar la función `length`.

```Haskell
import Data.List

string = "¡Hola! ¡Este es un blog post en español!"

-- Usamos la función length para encontrar la longitud de la cadena
length string -- Output: 41
```

En el código anterior, estamos asignando la cadena de texto a una variable llamada `string` y luego usando la función `length` para encontrar su longitud. La salida es `41`, ya que la cadena de texto contiene 41 caracteres incluyendo espacios y signos de puntuación.

También podemos usar `length` en cadenas de texto que contienen caracteres unicode.

```Haskell
string = "こんにちは世界"

length string -- Output: 6
```

Además, podemos usar `length` en listas de cadenas de texto.

```Haskell
strings = ["Hola", "Amigos", "en", "Haskell"]

length strings -- Output: 4
```

## Profundizando

La función `length` en Haskell es posible gracias a que el tipo de dato `String` se define como una lista de caracteres (`[Char]`). Por lo tanto, `length` simplemente cuenta el número de elementos en la lista.

Es importante tener en cuenta que la función `length` también funciona en otros tipos de datos, como listas de números.

```Haskell
numbers = [1, 2, 3, 4, 5]

length numbers -- Output: 5
```

También es posible crear una función propia para encontrar la longitud de una cadena de texto.

```Haskell
-- Creamos una función recursiva que cuenta los caracteres de una cadena
-- utilizando el patrón de plegado (folding)
customLength :: [a] -> Int
customLength = foldr (\_ acc -> acc + 1) 0

string = "¿Qué tal?"

customLength string -- Output: 8
```

La función `foldr` nos permite aplicar una función a cada elemento de una lista en orden inverso y acumular el resultado en un valor inicial, que en este caso es `0`.

## Ver también

- [Documentación de la función `length` en Haskell](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:length)
- [Tutorial de Haskell en español](https://leobenkel.com/haskell/capitulo1/)