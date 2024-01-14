---
title:                "Elm: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Por qué

A veces, en la programación, nos encontramos con situaciones en las que necesitamos eliminar caracteres específicos de una cadena de texto. Esto puede ser útil en diversas situaciones, como limpiar datos o validar el formato de una entrada de usuario. En Elm, existe una manera sencilla de realizar esta tarea: eliminar caracteres que coincidan con un patrón.

##Cómo hacerlo

Para eliminar caracteres según un patrón determinado, utilizaremos la función `String.filter`. Esta función toma dos argumentos: una función de comparación y una cadena de texto. La función de comparación debe devolver `True` si el caracter debe ser mantenido, o `False` si debe ser eliminado.

```Elm
import String

texto = "¡H!ol#a, mun!d#o!"
patrón = (\car -> car /= "#" && car /= "!")
String.filter patrón texto
```

El resultado de este código sería `"Hola, mundo"`, ya que hemos eliminado todos los caracteres que coinciden con el patrón de nuestro texto original.

##Profundizando

La función `String.filter` es muy útil para eliminar caracteres que no necesitamos en nuestra cadena de texto. Sin embargo, también podemos utilizarla para extraer caracteres que coinciden con un patrón específico. En lugar de utilizar `\car -> car /= "#" && car /= "!"` como función de comparación, podemos definir una función que devuelva `True` solo para los caracteres que queremos extraer.

Por ejemplo, si queremos extraer solo las letras de una cadena de texto, podemos usar la función `Char.isAlpha` que verifica si un caracter es una letra del alfabeto:

```Elm
texto = "¡H!ol#a, mun!d#o!"
patrón = Char.isAlpha
String.filter patrón texto
```

El resultado de este código sería `"Hola mundo"`, ya que hemos extraído solo las letras de nuestro texto original.

En resumen, la función `String.filter` nos permite eliminar o extraer caracteres de una cadena de texto según un patrón determinado, lo que puede ser muy útil en diversas situaciones de programación.

##Ver también

- Documentación oficial de Elm sobre la función `String.filter`: https://package.elm-lang.org/packages/elm/core/latest/String#filter
- Artículo en español sobre la función `String.filter`: https://jaxtary.com/es/blog/2017/08/strin-filter-funcion-eliminar-caracteres-elm.html