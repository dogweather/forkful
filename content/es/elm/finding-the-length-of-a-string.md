---
title:                "Encontrando la longitud de una cadena"
html_title:           "Elm: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

En Elm, la longitud de una cadena se refiere al número de caracteres que contiene. Los programadores a menudo necesitan encontrar la longitud de una cuerda para realizar operaciones en ella, como separarla en diferentes partes o realizar búsquedas en ella.

## Cómo:

Para encontrar la longitud de una cadena en Elm, podemos usar la función `String.length`, que acepta una cadena como argumento y devuelve su longitud como un número entero. Por ejemplo:

```Elm
String.length "Hola!" 
-- output: 5
```

Si queremos verificar si una cadena está vacía, simplemente podemos comparar su longitud con cero, como en el siguiente ejemplo:

```Elm
String.isEmpty "Hola!" 
-- output: False

String.isEmpty "" 
-- output: True
```

Si queremos encontrar la longitud de una cadena que contiene caracteres especiales, como emojis, podemos usar la función `String.length` de la misma manera y obtendremos el número correcto de caracteres.

## Buceo Profundo:

La función `String.length` en Elm está inspirada en la función `length` en otros lenguajes de programación, como Haskell y OCaml. Alternativamente, también podríamos usar el método `length` del paquete `String`, pero solo funcionará para cadenas de una sola línea.

En términos de implementación, la función `String.length` utiliza la función `length` internamente en el compilador de Elm, lo que asegura un rendimiento óptimo.

## Ver También:

Documentación oficial de Elm sobre la función `String.length`: https://package.elm-lang.org/packages/elm/string/latest/String#length

Para obtener más información sobre cadenas en Elm, consulte la siguiente fuente: https://guide.elm-lang.org/core_language.html#strings