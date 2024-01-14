---
title:                "Elm: Convirtiendo una cadena a minúsculas"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Es probable que si está leyendo este artículo, ya tenga alguna idea de por qué sería importante para usted convertir una cadena de texto a minúsculas. Sin embargo, en caso de que esté buscando razones adicionales, aquí hay algunas para considerar:

## ¿Por qué?

Pueden haber varias situaciones en las que necesite convertir una cadena de texto a minúsculas. Por ejemplo, si está trabajando con datos de entrada de un formulario, es posible que desee convertir todos los valores de texto a minúsculas antes de enviarlos al servidor para una comparación más fácil y precisa. También puede ser útil cuando se trabaja con datos de API o algoritmos de clasificación.

## ¿Cómo?

La conversión de una cadena de texto a minúsculas es un proceso sencillo en Elm. Puede hacerlo utilizando la función `String.toLower`, que toma una cadena de texto como argumento y devuelve una nueva cadena de texto en minúsculas. Aquí hay un ejemplo de código en Elm para demostrar cómo sería:

```Elm
name = "JUAN"
lowercaseName = String.toLower name
```

La salida de este código sería `"juan"`.

## Profundizando

Ahora que ya sabe cómo convertir una cadena de texto a minúsculas en Elm, puede ser curioso acerca de cómo funciona realmente esta función. Sin entrar demasiado en los detalles, la función `String.toLower` utiliza lo que se conoce como Unicode Character Database para determinar qué caracteres deben convertirse a minúsculas. Esto significa que, dependiendo de su idioma y los caracteres utilizados en su cadena de texto original, la conversión puede variar. Por ejemplo, si su cadena de texto original incluye la letra "Ñ", la función devolverá "ñ" en lugar de "n".

## Vea También

- [Documentación oficial de Elm: `String.toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Blog post en inglés: "A Comprehensive Guide to String Manipulation in Elm"](https://thoughtbot.com/blog/a-comprehensive-guide-to-string-manipulation-in-elm)