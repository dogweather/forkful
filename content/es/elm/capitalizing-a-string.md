---
title:                "Elm: Capitalizando una cadena"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué:

¿Alguna vez te has preguntado cómo puedes convertir una cadena de texto en mayúsculas en Elm? Aunque puede parecer algo sencillo, esta función puede ser muy útil en ciertas situaciones de programación. En este artículo, te explicaré cómo puedes capitalizar una cadena en Elm y por qué puede ser importante para tu código.

## Cómo hacerlo:

Para capitalizar una cadena en Elm, puedes utilizar la función `String.toUpper` que se encarga de convertir toda la cadena a mayúsculas. Por ejemplo, si tenemos la siguiente cadena de texto en nuestra variable `nombre`:

```Elm
nombre = "maría"
```

Podemos aplicar la función de esta manera:

```Elm
String.toUpper nombre
```

Esto producirá una salida de `"MARÍA"`, que es la versión en mayúsculas de nuestra cadena original. Ten en cuenta que esta función no cambiará la cadena original, sino que devolverá una nueva cadena en mayúsculas.

También puedes utilizar la función `String.toTitle` para capitalizar solo la primera letra de cada palabra en la cadena. Por ejemplo:

```Elm
String.toTitle nombre
```

Esto nos dará una salida de `"María"`. Otra opción es utilizar la función `String.toUpperFirst` para capitalizar solo la primera letra de la cadena. Por ejemplo:

```Elm
String.toUpperFirst nombre
```

Esto producirá una salida de `"MARÍA"`, ya que la primera letra de nuestra cadena original también se encuentra en mayúsculas.

## Inmersión profunda:

Puede parecer que capitalizar una cadena en Elm es un proceso simple, pero también es importante conocer los detalles detrás de esta función. Por ejemplo, si queremos capitalizar solo la primera letra de la cadena, podemos utilizar la función `String.toUpperFirst` como mencionamos anteriormente, pero también podemos utilizar la función `String.append` para unir la primera letra en mayúsculas con el resto de la cadena. Por ejemplo:

```Elm
String.toUpperFirst (String.append "m" "aría")
```

Esto también nos dará una salida de `"MARÍA"`. Además, si queremos capitalizar solo algunas palabras específicas en la cadena, podemos utilizar la función `String.split` para separar la cadena en una lista de palabras, aplicar la función `String.toUpperFirst` a las palabras que queramos capitalizar y luego unir la lista nuevamente en una cadena con la función `String.join`. Por ejemplo:

```Elm
String.join " " (List.map String.toUpperFirst (String.split " " nombre))
```

Esto capitalizará solo la primera letra de cada palabra en nuestra cadena. Estos son solo algunos de los ejemplos de cómo puedes profundizar en la función de capitalización de cadenas en Elm.

## Ver también:

- Documentación oficial de Elm sobre `String`: https://package.elm-lang.org/packages/elm/core/latest/String
- Tutorial de Elm en español: https://elm-tutorial.org/es/
- Ejemplos de código en Elm: https://github.com/M-C-Ra/ejemploElm