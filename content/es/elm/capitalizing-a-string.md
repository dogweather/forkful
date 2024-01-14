---
title:                "Elm: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Cuando se está programando en Elm, puede ser necesario capitalizar una cadena de texto por diferentes razones, como por ejemplo para mostrar nombres propios correctamente o para seguir ciertos estándares de escritura. En este blog post, vamos a explorar cómo se puede lograr esto utilizando Elm.

## Cómo hacerlo

Primero, es importante entender que en Elm, los strings son valores inmutables, lo que significa que no se pueden modificar directamente. Por lo tanto, para capitalizar una cadena, necesitamos crear una nueva cadena que contenga los caracteres en mayúsculas.

Una forma sencilla de hacerlo es utilizando la función `String.toUpper`, que toma un string como argumento y devuelve una nueva cadena con todos los caracteres en mayúsculas. Por ejemplo:

```Elm
String.toUpper "hola" -- devuelve "HOLA"
```

Sin embargo, esta función no capitaliza la primera letra de cada palabra en una oración. Para lograr esto, podemos utilizar la función `String.words` para dividir la cadena en palabras y luego la función `String.unwords` para unir las palabras nuevamente, utilizando `String.toUpper` en la primera letra de cada palabra. Ejemplo:

```Elm
String.unwords (List.map (\word -> String.toUpper (String.left 1 word) ++ String.dropLeft 1 word) (String.words "hola mundo")) -- devuelve "Hola Mundo"
```

Otra opción es utilizar la biblioteca [elm-community/string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/), que ofrece una función `capitalize` que capitaliza la primera letra de cada palabra en una cadena.

## Profundizando

Es importante tener en cuenta que estas opciones funcionan para cadenas simples, pero pueden no ser tan eficientes para cadenas más largas o complejas. En ese caso, es posible que se necesiten implementaciones más específicas o incluso crear una nueva función personalizada.

También es importante considerar que, como se mencionó anteriormente, las cadenas en Elm son valores inmutables, por lo que al aplicar una función de capitalización, se está creando una nueva cadena en lugar de modificar la existente. Esto puede ocasionar problemas de rendimiento en situaciones donde se trabaja con grandes cantidades de datos.

## Ver también

- [Documentación de Elm para la función String.toUpper](https://package.elm-lang.org/packages/elm/core/latest/String#toUpper)
- [Más opciones para el manejo de strings en Elm con elm-community/string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)