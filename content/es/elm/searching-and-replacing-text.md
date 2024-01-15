---
title:                "Búsqueda y reemplazo de texto"
html_title:           "Elm: Búsqueda y reemplazo de texto"
simple_title:         "Búsqueda y reemplazo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, es común tener que realizar cambios en texto para corregir errores o mejorar la estructura de un código. En estos casos, la búsqueda y reemplazo de texto es una herramienta importante y eficaz. En este artículo, aprenderemos cómo realizar esta tarea utilizando Elm.

## Cómo hacerlo
Para realizar una búsqueda y reemplazo en Elm, existen diferentes funciones que nos facilitan esta tarea. A continuación, veremos un ejemplo de cómo usarlas:

``` Elm
texto = "Hola mundo!"

-- Reemplazar el texto "mundo" por "Elm"
textoModificado = String.replace "mundo" "Elm" texto

-- Imprimimos el resultado
textomodificado
-- Retorna "Hola Elm!"
```

En este caso, utilizamos la función `String.replace` que toma tres argumentos: el texto a buscar, el texto por el cual se quiere reemplazar y el texto en el cual se realiza el reemplazo.

También podemos utilizar la función `String.replaceRegex` para realizar búsquedas y reemplazos utilizando expresiones regulares. A continuación, un ejemplo:

``` Elm
texto = "905-555-1234"

-- Reemplazar el formato de teléfono por uno más común
textoModificado = String.replaceRegex (Regex.regex "\\d{3}-\\d{3}") "905-555" texto

-- Imprimimos el resultado
textoModificado
-- Retorna "(905) 555-1234"
```

En este caso, utilizamos la función `Regex.regex` para crear una expresión regular que busca un formato de teléfono específico. Luego, utilizamos `String.replaceRegex` para reemplazar el texto que cumple con esa expresión.

## Profundizando
Además de las funciones mencionadas anteriormente, existen otras que nos pueden ser útiles al realizar una búsqueda y reemplazo de texto en Elm. Entre ellas se encuentran `String.split`, `String.join`, `String.toUpper` y `String.toLower`.

También es importante tener en cuenta que estas funciones en Elm son inmutables, es decir, no modifican el texto original sino que retornan una versión modificada del mismo. Esto es importante tenerlo en cuenta al trabajar con cadenas de texto en Elm.

## Ver también
- [Documentación oficial de la función String.replace](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- [Documentación oficial de la función String.replaceRegex](https://package.elm-lang.org/packages/elm/core/latest/String#replaceRegex)
- [Artículo sobre expresiones regulares en Elm](https://medium.com/@copperchicken/regular-expressions-in-elm-9f8d9f54700b)