---
title:                "Interpolando una cadena"
html_title:           "Elm: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Interpretar una cadena es un concepto importante en programación. Se refiere a la capacidad de insertar automáticamente valores variables en una cadena de texto. Los programadores lo hacen para hacer que sus cadenas sean más dinámicas y personalizables, lo que facilita la gestión de grandes cantidades de datos.

## Cómo:
```Elm
-- Ejemplo básico
mensaje = "¡Hola {nombre}!"
nombre = "Juan"
textoFinal = String.interpolate mensaje nombre
-- Output: "¡Hola Juan!" 
```
```Elm
-- Ejemplo con múltiples variables
texto = "El {color} coche {accion} en el {lugar}."
color = "rojo"
accion = "corre"
lugar = "parque"
textoFinal = String.interpolate texto (color, accion, lugar)
-- Output: "El rojo coche corre en el parque."
```

## Deep Dive:
- En la historia de la programación, la interpolación de cadenas se ha utilizado en diferentes lenguajes, como C, Python y JavaScript.
- Una alternativa a la interpolación de cadenas podría ser la concatenación de cadenas utilizando el operador "++". Sin embargo, esto puede volverse engorroso y difícil de leer, especialmente con cadenas más largas y con múltiples variables.
- En Elm, la interpolación de cadenas se logra utilizando la función preconstruida String.interpolate. Toma una cadena y una lista de valores y devuelve una cadena que reemplaza las variables indicadas con los valores correspondientes.

## Ver también:
- Documentación oficial de Elm sobre la interpolación de cadenas: https://package.elm-lang.org/packages/elm/core/latest/String#interpolate
- Un diálogo interesante sobre la decisión de Elm de no incluir la interpolación de cadenas en su sintaxis: https://discourse.elm-lang.org/t/why-is-there-no-string-interpolation-in-elm/5424/25