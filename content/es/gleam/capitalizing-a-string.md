---
title:                "Gleam: Mayusculas en una cadena"
simple_title:         "Mayusculas en una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En programación, es común encontrarnos con la necesidad de capitalizar una cadena de texto, es decir, convertir la primera letra de cada palabra en mayúscula. Esto puede ser útil en varios escenarios, como en la creación de nombres de usuarios, títulos de publicaciones o simplemente por motivos de estilo. Afortunadamente, en Gleam tenemos una función incorporada para realizar esta tarea de forma sencilla y eficiente.

## Cómo hacerlo

Para capitalizar una cadena en Gleam, utilizamos la función `String.capitalize()`. Veamos un ejemplo:

```Gleam
import gleam/string

let name = "maría"

let capitalizedName = string.capitalize(name)

// Output: María
```

Como se puede ver, simplemente tenemos que llamar a la función `capitalize()` y pasarle la cadena que deseamos capitalizar como argumento. La función devolverá una nueva cadena con la primera letra de cada palabra en mayúscula.

También es importante mencionar que si una palabra ya tiene la primera letra en mayúscula, la función la mantendrá de esa manera. Por ejemplo, si pasamos la cadena `Gleam Programming` como argumento, la función devolverá `Gleam Programming` sin modificarla.

## Profundizando

Detrás de la función `String.capitalize()` hay un algoritmo que realiza varias comprobaciones para determinar qué letras deben ser convertidas en mayúscula. Esto incluye tener en cuenta caracteres especiales y la posición de las palabras en la cadena. Es por eso que podemos confiar en que la función siempre hará su trabajo correctamente.

Además, como mencionamos anteriormente, la función devuelve una nueva cadena en lugar de modificar la original. Esto es importante si estamos trabajando con datos sensibles y no queremos alterarlos de forma permanente.

## Ver también

- [Documentación de Gleam sobre la función `String.capitalize()`](https://gleam.run/core/STRING.html#function.capitalize)
- [Ejemplo de uso de la función `String.capitalize()` en un proyecto real](https://github.com/username/proyecto-ejemplo)
- [Más recursos para aprender Gleam](https://gleam.run/learn/)