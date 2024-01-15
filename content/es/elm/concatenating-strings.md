---
title:                "Uniendo cadenas"
html_title:           "Elm: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué usar concatenación de cadenas en Elm?

La concatenación de cadenas en Elm es una forma eficiente y sencilla de combinar diferentes cadenas en una sola. Esto es especialmente útil cuando se trabaja con datos dinámicos y se necesita generar texto en función de esa información. La concatenación de cadenas permite crear mensajes personalizados y flexibles para los usuarios de tu aplicación.

## Cómo hacerlo en Elm

La sintaxis para concatenar cadenas en Elm es muy sencilla. Primero, declaras las cadenas que deseas combinar y luego utilizas el operador `++` para unirlos. Por ejemplo:

```Elm
nombre = "Juan"
apellido = "Pérez"
nombreCompleto = nombre ++ " " ++ apellido
```
El resultado de esto sería `"Juan Pérez"`, donde las dos cadenas se han combinado en una sola. También puedes concatenar más de dos cadenas a la vez utilizando el mismo operador.

## El detalle de la concatenación de cadenas

Cuando concatenas cadenas en Elm, es importante tener en cuenta que la operación se realiza de izquierda a derecha. Esto significa que si quieres añadir un prefijo o sufijo a una cadena, debes asegurarte de que esté en el lado correcto de la expresión `++`. Veamos un ejemplo:

```Elm
prefijo = "¡Hola "
mensaje = prefijo ++ "mundo!"
```
El valor de `mensaje` sería `"¡Hola mundo!"` ya que el prefijo se coloca delante de la cadena "mundo". Sin embargo, si escribimos la expresión inversa `mensaje = "mundo" ++ prefijo`, el resultado sería `"mundo¡Hola "`.

Otra cosa importante a tener en cuenta es que la concatenación de cadenas en Elm sólo funciona con cadenas de texto. Si intentas combinar una cadena con otro tipo de dato, como un número, obtendrás un error.

## Ver también

- Documentación oficial de Elm sobre concatenación de cadenas: https://guide.elm-lang.org/core_language.html#string-operations
- Explicación de cómo funciona el operador `++` en Elm: https://elixir-lang.org/getting-started/basic-types.html#string-interpolation-concatenation-and-replication