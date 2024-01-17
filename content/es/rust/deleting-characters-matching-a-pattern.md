---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Rust: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Eliminar caracteres que coinciden con un patrón es una tarea común en la programación. Consiste en eliminar de una cadena de texto los caracteres que cumplan con cierta condición, como por ejemplo, todos los números o todas las vocales. Los programadores realizan esta tarea para limpiar datos o para modificar una cadena de texto según ciertas reglas.

## ¿Cómo hacerlo?
En Rust, podemos usar el método `replace` de la clase `String` para eliminar caracteres que coinciden con un patrón. Veamos un ejemplo de cómo eliminar todos los números de una cadena de texto:

```Rust
let texto = "H0l4 m6nd0";
let nuevo_texto = texto.replace(|c: char| c.is_numeric(), "");
println!("{}", nuevo_texto);

// Output: Hl md
```

En este ejemplo, usamos el método `replace` pasándole como argumento una función que toma cada caracter de la cadena y devuelve `true` si es un número. En ese caso, ese caracter será reemplazado por una cadena vacía, eliminándolo así de la cadena original. El resultado será la cadena "Hl md", sin los números originales.

## Profundizando
Este tipo de manipulación de cadenas es una tarea común en la programación, por lo que existen diferentes formas de lograrlo en distintos lenguajes. En Rust, además del método `replace`, también podemos utilizar las funciones `trim`, `trim_start` y `trim_end` para eliminar caracteres de los extremos de una cadena de texto, y la macro `format!` para crear una nueva cadena a partir de una cadena original con ciertas modificaciones.

## Enlaces adicionales
Si quieres conocer más acerca de cómo trabajar con cadenas de texto en Rust, puedes leer la documentación oficial aquí: https://doc.rust-lang.org/std/string/. Además, aquí hay un artículo interesante sobre cómo manipular cadenas de texto en Rust: https://dev.to/kmamiya/working-with-strings-in-rust-11ho.