---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Haskell: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Si estás trabajando con Haskell, es muy probable que en algún momento necesites convertir una cadena de texto a minúsculas. Esto puede ser útil para muchas tareas, como validar entradas de usuario o comparar cadenas de manera más eficiente.

## Cómo hacerlo

La forma más sencilla de convertir una cadena a minúsculas en Haskell es usando la función `toLower` del módulo `Data.Char`. Aquí tienes un ejemplo de cómo usarla en una función:

```Haskell
import Data.Char (toLower)

toLowerString :: String -> String
toLowerString = map toLower
```

La función `toLowerString` usa la función `map` para aplicar `toLower` a cada elemento de la cadena, convirtiendo toda la cadena a minúsculas. Veamos cómo funciona con algunos ejemplos:

```Haskell
toLowerString "Hola Mundo!" -- devuelve "hola mundo!"
toLowerString "123 ABC" -- devuelve "123 abc"
toLowerString "Haskell" -- devuelve "haskell"
```

Otra forma de convertir una cadena a minúsculas es usando la función `map toLower` directamente en la cadena, en lugar de definir una función separada:

```Haskell
map toLower "Hola Mundo!" -- devuelve "hola mundo!"
```

Ambos métodos son válidos, pero es importante tener en cuenta que `toLower` solo funciona con letras del alfabeto, ignorando cualquier otro tipo de caracteres. Si necesitas convertir una cadena completamente a minúsculas, incluyendo caracteres especiales y números, puedes usar la función `map toLower` junto con `toCaseFold` del módulo `Data.Text`:

```Haskell
import Data.Char (toLower)
import Data.Text (toCaseFold)

toLowerString :: String -> String
toLowerString = unpack . toCaseFold . pack
```

Aquí hemos utilizado las funciones `pack` y `unpack` para convertir entre `String` y `Text`, ya que `toCaseFold` solo funciona con `Text`. Ahora, la función `toLowerString` funcionará con cualquier tipo de caracteres:

```Haskell
toLowerString "123 ABC" -- devuelve "123 abc"
toLowerString "HOLA#$Mundo" -- devuelve "hola#$mundo"
toLowerString "ĂȘȚĒ" -- devuelve "ășțē"
```

## Profundizando

En términos más técnicos, `toLower` y `toCaseFold` funcionan mediante el uso de la tabla de Unicode (un estándar que asigna un número a cada carácter utilizado en la mayoría de los sistemas de escritura). Esta tabla contiene información sobre cada carácter, incluyendo si es mayúscula, minúscula o una letra especial. Al usar estas funciones, Haskell busca en la tabla de Unicode para determinar si un carácter tiene una versión en minúsculas y, de ser así, la devuelve.

Por otro lado, si quieres convertir una cadena a mayúsculas, puedes usar la función `toUpper` del módulo `Data.Char`, que funciona de la misma manera que `toLower`, pero convirtiendo los caracteres a su versión en mayúsculas en lugar de minúsculas.

## Ver también

- [Documentación oficial de `Data.Char`](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Documentación oficial de `Data.Text`](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Unicode](https://www.unicode.org/)