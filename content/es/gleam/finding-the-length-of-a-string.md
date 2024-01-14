---
title:    "Gleam: Encontrar la longitud de una cadena"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo necesitamos saber la longitud de una cadena de texto para realizar ciertas operaciones. Ya sea para determinar cuántos caracteres contiene una palabra o para generar un error si la cadena es demasiado larga, encontrar la longitud de una cadena es una habilidad básica y útil en cualquier lenguaje de programación. En Gleam, este proceso es sencillo y eficiente.

## Cómo hacerlo

La función `string.length/1` es la forma más común de encontrar la longitud de una cadena en Gleam. Simplemente le pasamos la variable que contiene nuestra cadena y nos devuelve un número que representa la cantidad de caracteres. Veamos un ejemplo:

```Gleam
let name = "Juan"
let length = string.length(name)
```
El valor de `length` sería `4`, ya que hay cuatro caracteres en la cadena "Juan".

También podemos usar una cadena directamente en la función sin necesidad de asignarla a una variable primero:

```Gleam
let length = string.length("Hola!")
```
En este caso, `length` sería `5`, ya que hay cinco caracteres en la cadena "Hola!".

## Profundizando

Detrás de escena, la función `string.length/1` cuenta la cantidad de bytes que componen la cadena, en lugar de simplemente contar los caracteres visuales. Esto significa que algunas letras pueden tener más de un byte y, por lo tanto, pueden afectar la longitud de una cadena.

También es importante tener en cuenta que diferentes lenguajes pueden tener diferentes longitudes de cadena debido a la codificación de caracteres utilizada. Por ejemplo, una cadena en chino puede tener una longitud diferente a la misma cadena en inglés debido a la forma en que se codifican los caracteres en cada idioma.

## Ver también

- Documentación oficial de Gleam sobre `string.length/1`: https://gleam.run/docs/stdlib/string#length
- Un tutorial sobre operaciones básicas de cadenas en Gleam (en inglés): https://gleam.run/articles/strings

¡Con esta información, ya estás listo para utilizar la función `string.length/1` en tus proyectos Gleam! Recuerda tener en cuenta la codificación de caracteres y elegir la mejor forma de trabajar con cadenas según tus necesidades específicas. ¡Feliz programación!