---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Buscar y reemplazar texto es una función que permite localizar un string específico y sustituirlo por otro. Los programadores recurren a este método para modificar código de manera eficiente, evitar la redundancia y acelerar la corrección de errores.

## Cómo hacerlo:

Vamos a explorar cómo buscar y reemplazar texto en Rust. Asegúrate de haber instalado la última versión antes de iniciarlo.

```rust
fn main() {
     let texto = "Hola Mundo!";
     let resultado = texto.replace("Mundo", "Rust");
     println!("{}", resultado);
}
```

Cuando ejecutas este código, aparecerá el siguiente resultado:

```rust
"Hola Rust!"
```
En este ejemplo, hemos reemplazado "Mundo" por "Rust" en el string "Hola Mundo!".

## Inmersión Profunda

Buscar y reemplazar texto tiene sus raíces en el antiguo lenguaje de programación ed de Unix, y ha sido implementado en la mayoría de los lenguajes de programación modernos, incluido Rust.

Otros métodos para manipulaciones de texto en Rust incluyen push_str() para agregar strings, y trim() para quitar espacios en blanco. 

La implementación de buscar y reemplazar en Rust, utiliza el método replace() de la biblioteca estándar que realiza una búsqueda lineal, lo que significa que la eficacia del método decrece a medida que el tamaño del texto aumenta.

## Ver También

Rust tiene una amplia base de documentación, aquí te dejamos algunos enlaces para aprender más sobre manipulación de texto en Rust:

1. [Libro de programación Rust](https://doc.rust-lang.org/book/)
2. [Biblioteca estándar de Rust: std::string::String](https://doc.rust-lang.org/std/string/struct.String.html) 
3. [Documentación de Rust sobre el método replace()](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)