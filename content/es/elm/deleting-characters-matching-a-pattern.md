---
title:    "Elm: Borrando caracteres que coinciden con un patrón"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón puede ser una tarea útil al trabajar con texto en Elm. Puede ayudar a limpiar datos o a filtrar información de manera más precisa.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Elm, podemos utilizar la función `String.replace` y proporcionar el patrón y la cadena original. Por ejemplo:

```Elm
import String

phrase = "¡Hola! ¡Hola! ¡Adios!"
pattern = "¡Hola!"

String.replace pattern "" phrase

-- output: " ¡Adios!"
```

En este ejemplo, reemplazamos todas las apariciones de "¡Hola!" con una cadena vacía, lo que resulta en la eliminación de esos caracteres del texto original.

También podemos combinar `String.replace` con otras funciones, como `String.filter`, para eliminar solo ciertos caracteres que coinciden con un patrón. Por ejemplo:

```Elm
import String

phrase = "¡Hola mundo!"

pattern = "\\s"
onlyLetters = String.filter (\c -> c /= " ") phrase
withoutSpaces = String.replace pattern "" onlyLetters

-- output: "HOLAmundo!"
```

En este caso, primero filtramos los espacios en blanco utilizando el patrón "`\s`" que representa cualquier espacio en blanco. Luego, utilizamos `String.replace` para eliminar esos espacios del texto original.

## Profundizando más

Elm ofrece una variedad de funciones y métodos para trabajar con cadenas de texto y patrones. Algunas de ellas incluyen `String.split`, `String.startsWith`, `String.fromList`, entre otras. Estas funciones pueden ser útiles al crear algoritmos más complejos para eliminar caracteres que coinciden con un patrón específico. Es importante experimentar con estas funciones y encontrar la mejor manera de aplicarlas a nuestro código.

## Ver también

- [Documentación oficial de Elm sobre String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Tutorial de Elm por el canal de YouTube Fazt Code](https://www.youtube.com/watch?v=zFP4_ymEj14)
- [Ejemplos prácticos de uso de `String.replace`](https://korban.net/posts/elm/2018-07-12-slicing-up-text-with-elm-string-replace/)