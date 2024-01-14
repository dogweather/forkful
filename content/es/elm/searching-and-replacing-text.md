---
title:                "Elm: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, es común necesitar reemplazar una cadena de texto por otra. Ya sea para corregir un typo o para actualizar variables en un archivo, saber cómo hacer esto es una habilidad esencial para cualquier programador.

## Cómo hacerlo
Para realizar una búsqueda y reemplazo de texto en Elm, se puede utilizar la función `String.replace`. A continuación, mostraremos un ejemplo de cómo reemplazar la palabra "gato" por "perro" en una cadena de texto:

```Elm
cadena = "Me gustan los gatos"
nuevaCadena = String.replace "gato" "perro" cadena
```

En este caso, la variable `nuevaCadena` tendrá el valor "Me gustan los perros". Como se puede ver, la función `String.replace` toma tres argumentos: la palabra que queremos reemplazar, la nueva palabra y la cadena de texto en la que se realizará el reemplazo.

## Profundizando
Además de la función `String.replace`, existen otras formas de realizar una búsqueda y reemplazo de texto en Elm. Por ejemplo, se puede utilizar la función `String.replaceMany` para reemplazar múltiples palabras en una sola operación. Además, se pueden utilizar expresiones regulares para hacer búsquedas más complejas.

Para aprender más sobre cómo realizar búsquedas y reemplazos de texto en Elm, te recomendamos revisar la documentación oficial y experimentar con diferentes ejemplos.

## Ver también
- [Documentación oficial de Elm: String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Ejemplos prácticos de búsqueda y reemplazo en Elm](https://www.elm-tutorial.org/es/06-strings.html#search-and-replace)