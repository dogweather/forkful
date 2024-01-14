---
title:                "Elm: Uniendo cadenas"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

#¿Por qué deberías concatenar strings en Elm?

La concatenación de strings es una técnica esencial en cualquier lenguaje de programación para unir cadenas de texto y formar una sola cadena más larga. En Elm, puede resultar útil a la hora de crear mensajes de texto dinámicos en una interfaz de usuario o formar URLs personalizadas para llamar a una API externa, entre otros casos de uso.

## ¿Cómo hacerlo en Elm?

Para concatenar strings en Elm, utilizamos el operador `++`, que simplemente une dos cadenas de texto. Veamos algunos ejemplos prácticos:

```
-- Ejemplo 1: Concatenando dos strings
"¡Hola " ++ "amigo!"  --> "¡Hola amigo!"

-- Ejemplo 2: Uniendo más de dos strings
"Bienvenido " ++ "a " ++ "mi " ++ "blog"  --> "Bienvenido a mi blog"

-- Ejemplo 3: Usando variables y concatenación para formar una URL personalizada
let
  name = "Juan"
  age = 30
  url = "https://miapi.com/users/" ++ name ++ "?age=" ++ String.fromInt age
in
  url  --> "https://miapi.com/users/Juan?age=30"
```

## Profundizando en la concatenación de strings

En Elm, cada cadena de texto es en realidad un valor de tipo `String`, que es un listado de caracteres de texto. Al utilizar el operador `++`, lo que estamos haciendo es unir dos listas de caracteres para crear una sola lista más larga. Esto significa que también podemos concatenar listas de caracteres en lugar de strings individuales.

Además, es importante mencionar que el operador `++` es asociativo a la derecha, lo que significa que en una concatenación de tres o más strings, se unirán primero los últimos dos y luego el resultado con el siguiente. Por ejemplo:

```
"¡Hola " ++ "amigo" ++ "!"  --> "¡Hola amigo!"

equivale a:

"¡Hola " ++ ("amigo" ++ "!")  --> "¡Hola amigo!"
```

## Ver también

- [Documentación oficial sobre concatenación de strings en Elm](https://package.elm-lang.org/packages/elm/core/latest/String#++)
- [Tutorial sobre manipulación de strings en Elm](https://dev.to/sgrvl/tutorial-string-manipulation-in-elm-44jn)
- [Ejemplos de uso de concatenación de strings en Elm](https://github.com/w0rm/elm-string-concat/blob/master/Compare.md)