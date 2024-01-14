---
title:                "Swift: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por qué
Extraer subcadenas o "substrings" es una habilidad importante en Swift, ya que te permite manipular y obtener solo la información que necesitas de una cadena de texto más grande. Puede ser útil en situaciones como análisis de datos, procesamiento de texto o validación de entradas de usuario.

## Cómo hacerlo
Para extraer una subcadena de una cadena existente en Swift, puedes utilizar la función `substring(from:to:)`. Por ejemplo, si tienes la siguiente cadena:

```Swift
let texto = "¡Hola a todos!"
```

y quieres extraer solo la palabra "todos", puedes hacerlo así:

```Swift
let subcadena = texto.substring(from: 7, to: 12)
print(subcadena) // Esto imprimirá "todos"
```

También puedes utilizar el operador `[]` para extraer una subcadena. Por ejemplo:

```Swift
let otraSubcadena = texto[1...3]
print(otraSubcadena) // Esto imprimirá "ola"
```

Es importante tener en cuenta que en ambos casos, el índice de inicio es inclusivo y el índice final es exclusivo. Además, si no especificas un índice final, la subcadena se extenderá hasta el final de la cadena original.

## Profundizando
Las subcadenas en Swift tienen un tipo de dato propio, `Substring`, que es una vista de la cadena original. Esto significa que la subcadena comparte la misma memoria que la cadena original, lo que puede ser útil si necesitas mantener la eficiencia en tu código. Sin embargo, si necesitas una cadena independiente, deberás convertir la subcadena a `String` utilizando la función `String(substring)`.

También hay otras formas de extraer subcadenas en Swift, como utilizando expresiones regulares o funciones de búsqueda y reemplazo. Sin embargo, la función `substring(from:to:)` sigue siendo una de las formas más utilizadas y sencillas de extraer subcadenas en Swift.

## Ver también
- [Documentación oficial de Swift sobre la función substring](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID282)
- [Tutorial de Ray Wenderlich sobre cómo extraer subcadenas en Swift](https://www.raywenderlich.com/5442-how-to-use-strings-and-characters-in-swift)
- [Tutorial de Hacking with Swift sobre diferentes formas de trabajar con subcadenas en Swift](https://www.hackingwithswift.com/articles/136/how-to-work-with-substrings-in-swift)