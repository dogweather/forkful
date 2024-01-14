---
title:    "Elm: Capitalizando una cadena"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

La capitalización de una cadena de texto es una técnica común en la programación que puede ser útil en diversas situaciones, como formatear datos de entrada o mostrar información en pantalla de manera legible. En Elm, esta tarea puede lograrse de manera sencilla y eficiente con algunas funciones integradas.

## ¿Cómo hacerlo?

Para capitalizar una cadena de texto en Elm, se puede utilizar la función `String.capitalize`, que toma como argumento una cadena y devuelve una nueva cadena con la primera letra en mayúscula. Por ejemplo:

```Elm
String.capitalize "hola" -- Devuelve "Hola"
String.capitalize "elm" -- Devuelve "Elm"
```

Otra opción es utilizar la función `String.toPascalCase`, que capitaliza todas las palabras en una cadena, independientemente de su posición. Por ejemplo:

```Elm
String.toPascalCase "hola mundo" -- Devuelve "Hola Mundo"
String.toPascalCase "haciendo elm en español" -- Devuelve "Haciendo Elm en Español"
```

En caso de necesitar capitalizar únicamente la primera letra de cada palabra en una cadena, se puede utilizar la función `String.words` para dividir la cadena en palabras y luego aplicar la función `String.capitalize` a cada una de ellas. Por ejemplo:

```Elm
String.words "elm en español" |> List.map String.capitalize |> String.join " " -- Devuelve "Elm En Español"
```

## Profundizando

Existen otras funciones y técnicas para capitalizar cadenas de texto en Elm, como el uso de expresiones regulares o la implementación de una función personalizada para manejar casos más específicos. También es importante tener en cuenta que, al igual que otros lenguajes de programación, Elm cuenta con funciones para manipular mayúsculas y minúsculas a nivel de carácter, como `Char.toUpper` y `Char.toLower`.

En general, capitalizar una cadena de texto en Elm es una tarea sencilla que se puede realizar de manera rápida y eficiente con las funciones integradas en el lenguaje.

## Ver también

- [Documentación oficial de `String.capitalize`](https://package.elm-lang.org/packages/elm/core/latest/String#capitalize)
- [Tutorial sobre procesamiento de cadenas en Elm](https://www.elm-tutorial.org/en/03-subs-cmds/002-processing-strings.html)
- [Ejemplos de uso de expresiones regulares en Elm](https://elmprogramming.com/regular-expressions-in-elm.html)