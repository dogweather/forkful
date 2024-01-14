---
title:                "Rust: Extracción de subcadenas"
simple_title:         "Extracción de subcadenas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad esencial en la programación. Permite a los desarrolladores manipular y extraer información útil de una cadena de texto. En Rust, hay varias formas de realizar esta tarea, y en este artículo aprenderemos cómo hacerlo de manera eficiente.

## Cómo hacerlo

Primero, debemos importar la biblioteca estándar de Rust para manejo de cadenas de texto:

```Rust
use std::str;
```

Luego, podemos usar el método `get()` para extraer una subcadena específica de una cadena:

```Rust
let mensaje = "¡Hola, mundo!";
let subcadena = mensaje.get(6..11);
println!("{}", subcadena); // Salida: "mundo"
```

También podemos usar el método `chars()` para iterar a través de los caracteres de una cadena y extraerlos individualmente:

```Rust
let mensaje = "¡Hola, mundo!";
for c in mensaje.chars() {
  if c.is_alphabetic() {
    println!("{}", c); // Salida: "H", "o", "l", "a", "m", "u", "n", "d", "o"
  }
}
```

Existen otros métodos útiles para extraer subcadenas como `split()` para dividir una cadena en múltiples subcadenas y `trim()` para eliminar espacios en blanco al principio y al final de una cadena.

## Profundizando

Extraer subcadenas puede ser más complicado cuando se trata de cadenas que contienen caracteres Unicode. En tales casos, es importante comprender cómo funciona la codificación de caracteres y cómo afecta a la extracción de subcadenas.

En Rust, la biblioteca `unicode-segmentation` proporciona herramientas útiles para trabajar con cadenas Unicode y extraer subcadenas de manera adecuada. También es importante tener en cuenta el manejo de errores al realizar operaciones con subcadenas, ya que pueden surgir problemas con los índices fuera de rango.

## Ver también

- [Documentación oficial de cadenas en Rust](https://doc.rust-lang.org/std/string/index.html)
- [Ejemplos de manejo de subcadenas en Rust](https://doc.rust-lang.org/std/string/struct.String.html#examples)
- [Biblioteca unicode-segmentation en Rust](https://crates.io/crates/unicode-segmentation)