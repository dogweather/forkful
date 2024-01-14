---
title:    "Elm: Eliminación de caracteres que coinciden con un patrón"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coincidan con un patrón puede ser útil en situaciones en las que necesitamos limpiar datos o dar formato a una cadena de texto. Esto puede ahorrar tiempo y esfuerzo en comparación con hacerlo manualmente.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Elm, podemos utilizar la función `Regex.replace` de la biblioteca `elm/regex`. Pero antes de eso, necesitamos importar la biblioteca y definir nuestro patrón y cadena de texto. Luego, podemos utilizar `Regex.replace` para reemplazar los caracteres coincidentes con una cadena vacía.

```Elm
import Regex exposing (replace)

pattern = "[^a-zA-Z ]"
str = "¡Hola, ! ¿Cómo estás? ¡Soy un string con caracteres especiales!"

newStr = Regex.replace pattern str ""
```

Esto eliminará todos los caracteres que no sean letras o espacios en blanco de la cadena `str` y nos dará como resultado "Hola Como estás Soy un string con caracteres especiales".

También podemos utilizar grupos de captura en nuestro patrón y utilizarlos en la cadena de reemplazo. Por ejemplo, si queremos eliminar todos los números en una cadena de texto, podemos hacerlo de la siguiente manera:

```Elm
pattern = "\d+"
str = "¡Hola, 123! ¿Cómo estás? ¡Soy un string con números!"

newStr = Regex.replace pattern str "\1"
```

Esto eliminará todos los números de la cadena `str` y nos dará como resultado "¡Hola, ! ¿Cómo estás? ¡Soy un string con números!".

## Profundizando

Además de eliminar caracteres, también podemos utilizar `Regex.replace` para manipular y dar formato a cadenas de texto. Podemos utilizar expresiones regulares para buscar patrones específicos en una cadena y reemplazarlos con la información que deseemos.

Por ejemplo, si queremos dar formato a una fecha en formato `dd/mm/aaaa` a `aaaa-mm-dd`, podemos hacerlo utilizando grupos de captura y la función `Regex.replace`. Aquí hay un ejemplo de cómo podríamos hacerlo:

```Elm
pattern = "(\d{2})/(\d{2})/(\d{4})"
str = "Hoy es 31/05/2021"

formattedStr = Regex.replace pattern str "\3-\2-\1"
```

Esto nos dará como resultado "Hoy es 2021-05-31". Podemos jugar con diferentes patrones y cadenas de reemplazo para lograr el resultado deseado.

## Ver también

- Documentación de la biblioteca `elm/regex`: https://package.elm-lang.org/packages/elm/regex/latest/
- Tutorial de expresiones regulares en elm-explorations/test: https://elm-explorations.netlify.app/test#regexp
- Ejemplos de expresiones regulares en la documentación oficial de Elm: https://elm-lang.org/docs/regex