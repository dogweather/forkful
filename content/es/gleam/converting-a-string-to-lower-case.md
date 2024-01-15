---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Gleam: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

La conversión de una cadena a minúsculas es una tarea común en la programación, ya sea para normalizar entradas de usuario o para comparar cadenas de manera insensible a mayúsculas y minúsculas. Gleam proporciona una función integrada para realizar esta tarea de forma rápida y sencilla.

## Cómo hacerlo

Utilizando la función `String.to_lowercase()` de Gleam, podemos convertir una cadena a minúsculas de la siguiente manera:

```Gleam
let cadena = "HOLA MUNDO"
let nueva_cadena = String.to_lowercase(cadena)

assert nueva_cadena == "hola mundo"
```

El resultado de la función es una nueva cadena con todos los caracteres en minúsculas. Es importante tener en cuenta que esta función funciona con cualquier tipo de cadena, ya sea una variable o una literal.

También podemos encadenar esta función junto con otras operaciones en cadena para realizar varias transformaciones en una sola línea:

```Gleam
let nombre = "JUAN"
let saludo = "¡Hola, {String.to_lowercase(nombre)}!"

assert saludo == "¡Hola, juan!"
```

## Profundizando

Hay varias cosas importantes a tener en cuenta cuando se trabaja con la función `String.to_lowercase()` en Gleam. En primer lugar, esta función solo funciona con cadenas ASCII, lo que significa que solo puede convertir caracteres de la tabla ASCII a minúsculas. Si se intenta utilizar esta función en una cadena que contiene caracteres no ASCII, se producirá un error en tiempo de compilación.

Además, esta función no modifica la cadena original, sino que siempre devuelve una nueva cadena como resultado. Por lo tanto, si se desea modificar la cadena original, es importante asignar el resultado de la función a la misma variable.

Otra cosa a tener en cuenta es que esta función no solo convierte letras mayúsculas a minúsculas, sino que también reemplaza caracteres especiales como Ñ y Ü por sus equivalentes en minúsculas, ñ y ü respectivamente.

## Ver también

- Documentación oficial de la función `String.to_lowercase()`: https://gleam.run/modules/gleam_std.String.html#to_lowercase
- Ejemplos de uso de la función `String.to_lowercase()`: https://github.com/gleam-lang/gleam_stdlib/blob/master/lib/String.gleam#L78-L83