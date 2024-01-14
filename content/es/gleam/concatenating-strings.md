---
title:                "Gleam: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar cadenas de texto es una herramienta esencial en la programación. Permite combinar diferentes textos para crear uno más grande y completo. Aprender a concatenar correctamente las cadenas en Gleam puede ayudarte a mejorar tu eficiencia y a hacer tu código más legible.

## Cómo Hacerlo

Para concatenar cadenas en Gleam, usaremos el operador `++` entre dos cadenas. Veamos un ejemplo:

```Gleam
let saludo = "¡Hola"
let nombre = "María!"
let mensaje = saludo ++ nombre
```

Aquí, las cadenas "¡Hola" y "María!" se unen para formar un nuevo mensaje "¡Hola María!". Es importante tener en cuenta que el resultado de la concatenación será una nueva cadena, por lo que debemos asignarlo a una variable si queremos utilizarlo más adelante.

Si necesitamos agregar más cadenas al mensaje, podemos seguir utilizando el operador `++`:

```Gleam
let apellido = "González!"
let saludo_completo = mensaje ++ " " ++ apellido
```

Con esto, nuestro saludo ahora incluye el apellido de la persona, y tenemos el siguiente resultado: "¡Hola María González!".

## Profundizando

Es importante mencionar que el operador `++` solo puede unir dos cadenas juntas, por lo que si necesitas agregar más de dos cadenas, deberás utilizar una función llamada `String.concat`. Esta toma una lista de cadenas y las concatena en una sola. Veamos un ejemplo:

```Gleam
let texto1 = "¡Hola"
let texto2 = "a todos"
let texto3 = "los lectores!"
let mensaje = String.concat([texto1, texto2, texto3])
```

El resultado será la cadena "¡Hola a todos los lectores!".

## Ver También

- [Documentación oficial de Gleam sobre concatenación de cadenas](https://gleam.run/book/tour/string-and-ternary.html#concatenation)
- [Artículo sobre concatenación de cadenas en Gleam de Codeural](https://codeural.com/2020/08/10/concatenar-strings-en-gleam/)