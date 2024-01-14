---
title:                "Swift: Extrayendo subcadenas"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas o "substrings" es una tarea común en la programación, especialmente en Swift, para manipular y obtener información específica de una cadena de texto. Aprender a hacerlo puede facilitar y agilizar la escritura de código en ciertas situaciones.

## Cómo hacerlo

Para extraer una subcadena de una cadena de texto en Swift, utilizamos la función `substring (from: Int, to: Int)` donde `from` es el índice de inicio y `to` el índice de fin de la subcadena. Por ejemplo:

```Swift
let cadena = "¡Hola a todos!"
let subcadena = cadena.substring(from: 5, to: 7)
```

El resultado de `subcadena` sería la palabra "a". También podemos utilizar la función `substring (from: Int)` si solo queremos especificar el índice de inicio y obtener la subcadena desde ese punto hasta el final.

```Swift
let cadena = "Este es un ejemplo"
let subcadena = cadena.substring(from: 8)
```

El resultado de `subcadena` sería "un ejemplo".

## Profundizando

Además de las funciones básicas de `substring`, también existen otras formas de extraer subcadenas en Swift. Por ejemplo, podemos utilizar la función `prefix(_ maxLength: Int)` para obtener una subcadena de los primeros caracteres de una cadena de texto.

```Swift
let cadena = "¡Bienvenidos!"
let subcadena = cadena.prefix(5)
```

El resultado de `subcadena` sería "¡Bie". También podemos utilizar `suffix(_ maxLength: Int)` para obtener una subcadena de los últimos caracteres de una cadena de texto.

```Swift
let cadena = "¡Hola a todos!"
let subcadena = cadena.suffix(5)
```

El resultado de `subcadena` sería "todos!".

## Ver también

- [Documentación de Apple sobre `substring`](https://developer.apple.com/documentation/swift/string/3026561-substring)
- [Tutorial de Ray Wenderlich sobre manipulación de cadenas en Swift](https://www.raywenderlich.com/5006816-swift-5-0-strings-tutorial-pt-2-unicode)