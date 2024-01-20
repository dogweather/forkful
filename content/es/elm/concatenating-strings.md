---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

La concatenación de cadenas es situar una cadena de texto junto a otra para formar una sola. Los programadores lo hacen para combinar texto de manera dinámica, a menudo al interactuar con datos de usuario.

## Cómo se hace:

Elm proporciona el operador (++) para concatenar cadenas de texto. Aquí tienes un ejemplo:

```Elm
nombre = "Juan"
saludo = "Hola, " ++ nombre ++ "!"
```

Salida:

```Elm
"Hola, Juan!"
```

El ++ toma dos argumentos de tipo String y los une juntos.

## Un Vistazo Más de Cerca:

La concatenación de cadenas ha sido una funcionalidad básica en lenguajes de programación desde sus inicios. En Elm, la concatenación de cadenas se realiza a través del operador (++) en lugar de utilizar algún tipo de función de concatenación en comparación con otros lenguajes.

En cuanto a alternativas, Elm proporciona la función `String.concat` para unir una lista de cadenas en una sola cadena.

Elm maneja cadenas como listas de caracteres bajo el capó. Entonces, cuando concatenas cadenas, Elm está esencialmente uniendo estas listas internas.

```Elm
String.concat ["Hola, ", "Juan", "!"]
```

Salida:

```Elm
"Hola, Juan!"
```

## Ver También:

Para más detalles, consulta la [documentación oficial de String](https://package.elm-lang.org/packages/elm/core/latest/String) de Elm. Para una visión más completa de las capacidades de las cadenas de texto en Elm, te recomiendo que consultes [Guía Práctica de Elm: Cadenas de Texto](https://practical-elm.com/strings).