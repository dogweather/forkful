---
title:    "Elm: Convirtiendo una cadena a minúsculas"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos manipular cadenas de texto para realizar ciertas tareas. Una de ellas es convertir una cadena de texto a minúsculas. Esto es útil para comparar cadenas de texto de manera más precisa o para formatear datos de manera consistente.

## Cómo hacerlo

En Elm, podemos usar la función `String.toLower` para convertir una cadena de texto a minúsculas. Veamos un ejemplo:

```Elm
import String exposing (toLower)

resultado = toLower "ELM" 
```

El resultado de este código será `"elm"`. Como se puede ver, la cadena de texto "ELM" ha sido convertida a minúsculas usando la función `String.toLower`.

También podemos convertir una variable que contenga una cadena de texto a minúsculas de la siguiente manera:

```Elm
import String exposing (toLower)

cadena = "eLM"

resultado = toLower cadena
```

En este caso, el resultado también será `"elm"`, ya que la función `toLower` puede recibir una variable como argumento.

## Profundizando

Cuando convertimos una cadena de texto a minúsculas, debemos tener en cuenta que esto solo funciona correctamente con los caracteres del alfabeto inglés. Es decir, si tenemos palabras con acentos o caracteres especiales, estos no serán convertidos a minúsculas.

También es importante mencionar que esta función no altera la cadena de texto original, sino que devuelve una nueva cadena en minúsculas. Por lo tanto, si deseamos guardar el resultado en una variable, debemos asignarla a una nueva variable.

En resumen, la función `String.toLower` es útil para convertir cadenas de texto a minúsculas, pero debemos ser conscientes de sus limitaciones y cómo manejar el resultado correctamente.

## Ver también

- [Documentación oficial de la función `String.toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Tutorial de Elm en español](https://github.com/lucacavallin/taller-elm)