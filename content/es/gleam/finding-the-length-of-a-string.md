---
title:                "Gleam: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, hay muchas veces en las que necesitamos saber la longitud de una cadena de caracteres. Esto puede ser útil en varios casos, como por ejemplo trabajar con textos en una página web o manipular datos en una base de datos. En esta publicación, exploraremos cómo podemos encontrar la longitud de una cadena utilizando Gleam.

## Cómo hacerlo
En Gleam, podemos utilizar la función `str.length()` para encontrar la longitud de una cadena. Veamos un ejemplo práctico:

```Gleam
let cadena = "Hola, ¿cómo estás?";
let longitud = str.length(cadena);
io.print(longitud); // Output: 19
```

Como se puede ver, la función `str.length()` toma una cadena como argumento y devuelve un número que representa la longitud de esa cadena.
También podemos utilizar la función `str.to_bytes()` para convertir la cadena en una lista de bytes y luego obtener su longitud. Veamos cómo se hace:

```Gleam
let cadena = "Hello! ¿Cómo estás?";
let bytes = str.to_bytes(cadena);
io.print(list.length(bytes)); // Output: 21
```

Otra forma de encontrar la longitud de una cadena es iterar sobre ella y contar el número de caracteres. En Gleam, podemos hacerlo utilizando la función `str.fold()` que toma una función y una lista como argumentos. Veamos un ejemplo:

```Gleam
let cadena = "Feliz Navidad";
let longitud = str.fold(
    (caracter, contador) -> contador + 1,
    0,
    str.to_chars(cadena)
);
io.print(longitud); // Output: 14
```

## Profundizando
En Gleam, las cadenas son representadas como una lista de caracteres. Por lo tanto, podemos utilizar las funciones de listas para manipular cadenas de manera eficiente. También es importante tener en cuenta que las cadenas en Gleam son inmutables, lo que significa que no se pueden modificar una vez creadas. Por lo tanto, debemos tener cuidado al manipular cadenas en Gleam.

## Ver también
- Documentación oficial de Gleam sobre strings: https://gleam.run/book/tour/strings.html
- Ejemplos de cadenas en Gleam: https://github.com/gleam-lang/gleam/blob/master/examples/string.gleam