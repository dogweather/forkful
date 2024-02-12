---
title:                "Usando expresiones regulares"
aliases:
- /es/elm/using-regular-expressions/
date:                  2024-02-03T19:16:31.521978-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expresiones regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Las expresiones regulares (regex) en programación son patrones utilizados para emparejar combinaciones de caracteres en cadenas de texto. En Elm, al igual que en otros lenguajes, los programadores utilizan regex para tareas como validar entrada, buscar y reemplazar texto dentro de cadenas debido a su flexibilidad y eficiencia.

## Cómo hacerlo:
Elm no tiene funciones regex incorporadas en su librería principal, lo que requiere el uso de librerías de terceros para estas operaciones. Una de las opciones populares para trabajar con regex es `elm/regex`. Puedes agregarlo a tu proyecto usando `elm install elm/regex`.

Aquí te mostramos cómo puedes usar `elm/regex` para algunas tareas comunes:

### 1. Coincidencia de patrones
Para verificar si una cadena coincide con un patrón, puedes usar `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Ejemplo de uso:
isAlphanumeric "Elm2023"     -- Salida: True
isAlphanumeric "Elm 2023!"   -- Salida: False
```

### 2. Encontrar todas las coincidencias
Para encontrar todas las ocurrencias de un patrón dentro de una cadena, puedes utilizar `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Ejemplo de uso:
getWords "¡Elm es divertido!"  -- Salida: ["Elm", "es", "divertido"]
```

### 3. Reemplazar texto
Para reemplazar partes de una cadena que coincidan con un patrón, usas `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Ejemplo de uso:
replaceElmWithHaskell "¡Aprender Elm es divertido!"  
-- Salida: "¡Aprender Haskell es divertido!"
```

En estos ejemplos, `Regex.fromString` se utiliza para compilar un patrón regex, donde `\b` coincide con límites de palabra y `\w` coincide con cualquier carácter de palabra. Siempre maneja el resultado `Maybe` de `Regex.fromString` para protegerte contra patrones regex inválidos, típicamente usando `Maybe.withDefault`.
