---
title:                "Encontrar la longitud de una cadena"
html_title:           "Elm: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué alguien se involucraría en encontrar la longitud de una cadena?

Hay muchas razones por las que alguien puede querer encontrar la longitud de una cadena. Puede ser necesario para validar la entrada del usuario, manipular datos en una aplicación o simplemente obtener información sobre una cadena en particular. ¡Vamos a explorar cómo hacerlo en Elm!

## Cómo hacerlo

```Elm
-- Ejemplo 1: Usando la función `String.length`

cadena = "Hola Mundo"
longitudCadena = String.length cadena

-- La salida será 10
```

```Elm
-- Ejemplo 2: Usando la función `List.length` después de convertir la cadena en una lista de caracteres

cadena = "Hello World"
listaCaracteres = String.toList cadena
longitudCadena = List.length listaCaracteres

-- La salida será 11
```

También es importante tener en cuenta que los espacios en blanco y los signos de puntuación también contabilizarán como caracteres en la longitud de la cadena. 

## Profundizando

En Elm, una cadena es simplemente una lista de caracteres. Al usar la función `toList`, podemos convertir una cadena en una lista de caracteres y luego usar la función `List.length` para obtener la cantidad de caracteres en esa cadena. También podemos usar la función `String.length` directamente en una cadena sin necesidad de convertirla en una lista primero. 

Otro dato curioso es que, en Elm, los emojis también se consideran caracteres y contarán en la longitud de la cadena.

## Ver también

- Documentación oficial de Elm sobre la función `String.length`: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Documentación oficial de Elm sobre la función `List.length`: https://package.elm-lang.org/packages/elm/core/latest/List#length