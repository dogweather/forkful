---
title:                "Gleam: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Convertir una cadena de texto a minúsculas puede ser una tarea común en la programación. Al hacerlo, podemos asegurarnos de que los datos de entrada estén en un formato consistente y facilitar su manipulación en nuestro código.

## ¿Cómo hacerlo?

En Gleam, podemos usar la función `String.to_lower` para convertir una cadena de texto a minúsculas. Veamos un ejemplo:

```Gleam
let name = "JUAN"
let name_lower = String.to_lower(name)
```

El resultado sería `name_lower = "juan"`. Como puedes ver, la función `String.to_lower` transforma todas las letras en minúsculas. También podemos usar esta función con cadenas de texto más largas:

```Gleam
let sentence = "HOLA, ESTE ES UN EJEMPLO"
let sentence_lower = String.to_lower(sentence)
```

Y el resultado sería `sentence_lower = "hola, este es un ejemplo"`. Fácil, ¿verdad?

## ¡Profundicemos!

Ahora que sabemos cómo convertir una cadena de texto a minúsculas en Gleam, podemos compartir algunos consejos útiles. Primero, es importante tener en cuenta que la función `String.to_lower` solo funciona con letras en inglés. Si estás trabajando con letras acentuadas u otros caracteres especiales, puede que necesites usar una función diferente o codificar tus propios métodos de conversión.

Además, si necesitas transformar las letras en mayúsculas, Gleam también tiene una función llamada `String.to_upper` que hace exactamente lo contrario. También puedes usar la función `String.to_title` para convertir la primera letra de cada palabra en mayúsculas.

Finalmente, es importante mencionar que estas funciones son inmutables, lo que significa que no cambian el valor original de la cadena de texto. En su lugar, crean una nueva cadena de texto con los cambios aplicados. Así que asegúrate de asignar el resultado de estas funciones a una nueva variable.

## Ver También
- [Documentación de String.to_lower en Gleam](https://gleam.run/modules/text.html#to_lower)
- [Ejemplos prácticos de cómo usar String.to_lower](https://gist.github.com/juan/example)

¡Esperamos que este artículo te haya ayudado a aprender cómo convertir una cadena de texto a minúsculas en Gleam! Recuerda siempre consultar la documentación oficial y buscar ejemplos en la comunidad para hacer tu experiencia de programación en Gleam más fácil y divertida. ¡Feliz código!