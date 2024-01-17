---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Haskell: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La eliminación de caracteres que coinciden con un patrón es una técnica común en la programación para eliminar ciertos caracteres específicos de una cadena de texto. Los programadores utilizan esta técnica para limpiar y formatear datos, haciendo que la información sea más legible para las computadoras y los humanos.

## Cómo:

```Haskell
-- Eliminar todas las letras minúsculas de una cadena
deleteLowercase :: String -> String
deleteLowercase = filter (not . isLower)
```

** Input: ** "Hola Mundo"
** Output: ** "HM"

```Haskell
-- Eliminar todos los dígitos de una cadena
deleteDigits :: String -> String
deleteDigits = filter (not . isDigit)
```

** Input: ** "123abc456def"
** Output: ** "abcdef"

## Profundizando:

La eliminación de caracteres que coinciden con un patrón se ha utilizado desde los primeros días de la programación para manipular y limpiar datos. Antes de que existieran funciones de alto nivel en los lenguajes de programación, los programadores tenían que utilizar bucles y condicionales para realizar esta tarea.

Hoy en día, hay alternativas a la eliminación de caracteres que coinciden con un patrón, como el uso de expresiones regulares o funciones de biblioteca especializadas. Estas alternativas ofrecen una mayor flexibilidad y facilidad de uso, pero no siempre son tan eficientes como la eliminación de caracteres directa.

En Haskell, la función `filter` es la que se encarga de la eliminación de caracteres que coinciden con un patrón. Utiliza la función `isLower` y `isDigit` para comprobar si un carácter es una letra minúscula o un dígito, y luego filtra la lista de caracteres en función de eso. Esta implementación es bastante sencilla y eficiente, lo que la hace ideal para su uso en la programación diaria.

## Ver también:

- [Documentación oficial de filter en Haskell] (https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:filter)
- [Tutorial sobre expresiones regulares en Haskell] (https://wiki.haskell.org/Regular_expressions)
- [Librería de funciones para manipulación de cadenas en Haskell] (https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text-Internal.html)