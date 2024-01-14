---
title:                "Haskell: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

Las expresiones regulares son una herramienta esencial en la programación moderna. Permiten hacer búsquedas y manipulaciones avanzadas en texto de manera eficiente y precisa. Además, su uso es muy versátil y se pueden aplicar en una amplia gama de lenguajes de programación, incluyendo Haskell.

## Cómo usarlas en Haskell

Primero, debemos importar el módulo `Text.Regex.Posix` utilizando el comando `import Text.Regex.Posix`.

Luego, podemos usar la función `match` para buscar una expresión regular en una cadena de texto específica. Por ejemplo, si queremos encontrar todos los números de teléfono en una cadena, podríamos hacer lo siguiente:

```Haskell
import Text.Regex.Posix

let text = "Mi número de teléfono es 555-123-4567. ¿Cuál es el tuyo?"
let pattern = "([0-9]{3})-([0-9]{3})-([0-9]{4})"
let phoneNumbers = text =~ pattern :: [[String]]

-- Output:
-- [["555-123-4567", "555", "123", "4567"]]
```

En este ejemplo, la función `match` nos devuelve una lista de listas de cadenas, donde cada sublista contiene las coincidencias encontradas en la cadena, siguiendo el orden especificado en el patrón.

También podemos utilizar expresiones regulares para reemplazar texto en una cadena. Por ejemplo, si queremos ocultar los dígitos de un número de tarjeta de crédito en una cadena, podríamos hacer lo siguiente:

```Haskell
import Text.Regex.Posix

let text = "Mi número de tarjeta de crédito es 1234 5678 9012 3456"
let pattern = "(\\d{4})\\s?(\\d{4})\\s?(\\d{4})\\s(\\d{4})"
let hiddenText = text =~ pattern :: String

-- Output:
-- "Mi número de tarjeta de crédito es **** **** **** 3456"
```

Aquí, utilizamos `\\s` para indicar que podemos tener o no un espacio entre cada grupo de cuatro dígitos en el patrón.

## Profundizando en el uso de expresiones regulares

Existen muchas más funciones disponibles en el módulo `Text.Regex.Posix` que nos permiten realizar operaciones más avanzadas con expresiones regulares. Además, existen diferentes tipos de expresiones, como las expresiones regulares básicas (POSIX) y las expresiones regulares POSIX extendidas.

Es importante leer la documentación oficial y explorar diferentes ejemplos para entender mejor cómo utilizar expresiones regulares en Haskell y aprovechar al máximo su potencial.

## Ver también

- [Documentación oficial de `Text.Regex.Posix`](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
- [Tutorial de expresiones regulares en Haskell](https://www.tutorialspoint.com/haskell/haskell_regular_expressions.htm)
- [The Power of Regular Expressions in Haskell](https://dev.to/cmelgarejo/the-power-of-regular-expressions-in-haskell-41m)