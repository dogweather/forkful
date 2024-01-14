---
title:    "Elm: Capitalizando una cadena de texto."
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

La capitalización de una cadena de texto puede ser útil en situaciones en las que necesitamos resaltar ciertos aspectos de una palabra o frase. Por ejemplo, cuando queremos enfatizar un título o un nombre propio en un texto.

## Cómo hacerlo

La programación en Elm ofrece una función incorporada para capitalizar una cadena de texto: `String.capitalize`. Simplemente debemos pasar la cadena como argumento y la función devolverá la misma cadena con la primera letra en mayúscula.

```Elm
String.capitalize "hola" -- retorna "Hola"
String.capitalize "elm programming" -- retorna "Elm programming"
```

Otra opción es utilizando la función `String.toUpper` en combinación con `String.slice` para obtener solo la primera letra en mayúscula y luego unirlo con el resto de la cadena.

```Elm
String.toUpper (String.slice 0 1 "hola") ++ String.slice 1 4 "hola" -- retorna "Hola"
String.toUpper (String.slice 0 1 "elm programming") ++ String.slice 1 5 "elm programming" -- retorna "Elm programming"
```

## Profundizando

La función `String.capitalize` en realidad aplica la regla de capitalización del idioma inglés, es decir, solo la primera letra será mayúscula, independientemente de la posición de la palabra en la oración. También considera las excepciones de capitalización, como "Mc" o "Mac". Por ejemplo:

```Elm
String.capitalize "mcdonald's" -- retorna "McDonald's"
```

Si queremos aplicar reglas de capitalización diferentes, por ejemplo, en un idioma diferente, podemos escribir nuestra propia función personalizada utilizando `String.toUpper` y `String.slice` como se mostró anteriormente.

## Ver también

- Documentación oficial de Elm sobre `String.capitalize`: https://package.elm-lang.org/packages/elm/core/latest/String#capitalize
- Domingo Galdos, "Aprende Elm: Capitalizar una cadena": https://domingogaldos.com/tutoriales/aprende-elm-capitalizar-una-cadena/