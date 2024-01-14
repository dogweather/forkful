---
title:                "Elm: Convertir una cadena a minúsculas"
simple_title:         "Convertir una cadena a minúsculas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué 
No es ningún secreto que la programación puede ser difícil de entender para muchas personas. Una de las cosas que puede ser especialmente complicada es la manipulación de cadenas de texto. Sin embargo, en Elm, hay una función simple para convertir una cadena a minúscula que puede ahorrar tiempo y esfuerzo a los programadores. A continuación, explicaré cómo utilizar esta función para facilitar su trabajo.

## Cómo hacerlo 
Utilizando la función `String.toLower` en Elm, podemos convertir una cadena de texto a su equivalente en minúscula. Veamos un ejemplo de cómo usar esta función en un archivo `Main.elm`.

```Elm
import Html

main =
  Html.text (String.toLower "ELM PROGRAMMING IS GREAT!")
```

El resultado de este código sería "elm programming is great!" en la pantalla. Como puede ver, la función `String.toLower` toma una cadena de texto y la convierte a su versión en minúscula. Esto puede ser especialmente útil cuando se trabaja con entradas de usuarios, ya que permite que el programa sea más flexible al reconocer tanto mayúsculas como minúsculas.

También podemos combinar la función `String.toLower` con otras funciones, como `String.trim` para eliminar los espacios en blanco de una cadena antes de convertirla a minúscula. Veamos cómo sería esto en un código completo:

```Elm
import String exposing (trim, toLower)
import Html

main =
  let
    input = "   Elm   Programming    "
  in
    Html.text (toLower (trim input))
    
-- El resultado será "elm programming" en la pantalla 
```

## Profundizando 
Para aquellos interesados en saber más sobre cómo funciona la función `String.toLower` en Elm, aquí hay algunos detalles adicionales. En realidad, esta función está creada a partir de otras funciones, en lugar de ser implementada directamente. En primer lugar, la cadena se convierte en una lista utilizando la función `String.toList`. Luego, cada elemento de esta lista se convierte a su equivalente en minúscula utilizando la función `Char.toLower`. Por último, la lista se convierte de nuevo en una cadena utilizando la función `String.fromList`. Esto permite una mayor flexibilidad y personalización al trabajar con cadenas de texto.

## Ver también 
* Documentación oficial sobre `String.toLower`: https://package.elm-lang.org/packages/elm/core/1.0.2/String#toLower
* Tutorial interactivo para aprender Elm: https://elmprogramming.com/learn/EbyrBzVJshoc9XGh0Vvt
* Ejemplos adicionales de uso de la función `String.toLower`: http://elm-lang.org/docs/from-javascript#lowercase-a-string