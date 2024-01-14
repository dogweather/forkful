---
title:    "Elm: Buscando y reemplazando texto"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has preguntado alguna vez por qué es necesario realizar búsquedas y reemplazos en tus textos de programación? La respuesta es simple: para ahorrar tiempo y evitar errores. Con la función de búsqueda y reemplazo, puedes modificar rápidamente una parte del código en lugar de tener que hacer cambios manualmente en cada línea. Sigue leyendo para aprender cómo hacerlo en Elm.

## Cómo hacerlo

La sintaxis para realizar una búsqueda y reemplazo en Elm es la siguiente:

```Elm
Text.replace oldText newText texto
```

Donde "oldText" es la parte del texto que quieres buscar y "newText" es la parte con la que quieres reemplazarlo. El último parámetro "texto" es el texto en el que se realizará la búsqueda y reemplazo.

Veamos un ejemplo concreto. Si queremos reemplazar la palabra "hola" por "adios" en el siguiente texto:

```Elm
"Hola, ¿cómo estás?"
```

El código que usaríamos sería:

```Elm
Text.replace "hola" "adios" "Hola, ¿cómo estás?"
```

Y el resultado sería:

```Elm
"Adios, ¿cómo estás?"
```

También se pueden realizar búsquedas y reemplazos en listas de textos. Por ejemplo, si tenemos la siguiente lista:

```Elm
["Hello", "Hola", "Bonjour"]
```

Y queremos reemplazar la palabra "Hola" por "Salut", podemos utilizar la función "map" junto con "Text.replace":

```Elm
List.map (Text.replace "Hola" "Salut") ["Hello", "Hola", "Bonjour"]
```

Y el resultado sería:

```Elm
["Hello", "Salut", "Bonjour"]
```

## Profundizando

En Elm, la función "Text.replace" utiliza expresiones regulares para realizar la búsqueda y reemplazo. Esto significa que puedes utilizar patrones o caracteres especiales para realizar búsquedas más complejas. Por ejemplo, si queremos reemplazar cualquier vocal con "x" en el siguiente texto:

```Elm
"¡Hola Mundo!"
```

Podríamos utilizar el siguiente código:

```Elm
Text.regexReplace (Regex.regex "[aeiou]") ((\_ -> "x")) "¡Hola Mundo!"
```

Y el resultado sería:

```Elm
"¡Hxlx Mxndx!"
```

## Ver también

- Documentación oficial de la función Text.replace en Elm: https://package.elm-lang.org/packages/elm/core/latest/Text#replace
- Ejemplos y explicación sobre expresiones regulares en Elm: https://elmprogramming.com/regex-in-elm.html
- Otras funciones de manipulación de textos en Elm: https://www.brianthicks.com/guide/elm/strings.html