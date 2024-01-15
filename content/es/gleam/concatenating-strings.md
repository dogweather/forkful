---
title:                "Uniendo cadenas de texto"
html_title:           "Gleam: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
Las cadenas de texto (strings) son una parte fundamental de la programación, ya que nos permiten mostrar información y comunicarnos con el usuario. Al concatenar (unir) strings, podemos crear mensajes personalizados y manipular la información de manera más eficiente.

## Cómo
La sintaxis para concatenar strings en Gleam es muy sencilla. Podemos utilizar el operador `++` para unir dos o más strings juntos. Veamos un ejemplo:

```Gleam
let saludo = "¡Hola, "
let nombre = "Juan!"
let mensaje = saludo ++ nombre
```

En este caso, la variable mensaje contendrá el valor "¡Hola, Juan!", ya que el operador `++` ha concatenado las dos strings.

También podemos utilizar la función `concat` para concatenar más de dos strings a la vez. Por ejemplo:

```Gleam
let saludo = "¡Hola, "
let nombre = "Juan!"
let apellido = "Pérez!"
let mensaje = concat(saludo, nombre, apellido)
```

En este caso, la variable mensaje también contendrá el valor "¡Hola, Juan Pérez!".

Además, podemos utilizar placeholders o marcadores de posición en las strings, que luego serán sustituidos por valores específicos. Esto es muy útil para crear mensajes dinámicos. Veamos un ejemplo:

```Gleam
let saludo = "¡Hola, %s!"
let nombre = "Juan"
let mensaje = saludo ++ nombre
```

En este caso, al imprimir la variable mensaje, el output será "¡Hola, Juan!", ya que el marcador de posición `%s` se ha sustituido por el valor de la variable nombre.

## Deep Dive
En Gleam, las strings son representadas internamente como listas de caracteres, por lo que podemos utilizar funciones de listas para manipularlas. Por ejemplo, la función `length` nos devuelve la longitud de una string, la función `reverse` nos permite invertir el orden de los caracteres y la función `map` nos permite aplicar una función a cada elemento de la string.

Además, también podemos utilizar el operador `<<` para insertar una string en otra en una posición específica. Por ejemplo:

```Gleam
let texto = "Este es un texto"
let subtexto = " un"
let mensaje = texto << subtexto << " ejemplo."
```

En este caso, la variable mensaje contendrá el valor "Este es un ejemplo.", ya que hemos insertado la string subtexto después de la palabra "es".

## See Also
- [Documentación oficial de Gleam sobre strings](https://gleam.run/book/core_types-strings.html)
- [Tutorial de concatenación de strings en Gleam](https://gleam.run/blog/concatenating-strings-gleam.html)