---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Longitud de una Cadena en Rust

## ¿Qué y por qué?

Mensurar la longitud de una cadena (es decir, contar sus caracteres) en la programación es esencial. Te ayuda a manipular cadenas eficientemente. Por ejemplo, este dato puede ser útil en bucles, validación, entre otras tareas.

## Cómo hacerlo:

En Rust, se determina la longitud de una cadena usando el método `.len()`. Echemos un vistazo a un ejemplo en el que demostraremos cómo medir el tamaño de una cadena.

```Rust
fn main() {
    let cadena = "Hola Mundo";
    println!("Longitud: {}", cadena.len());
}
```

Este bloque de código imprimirá "Longitud: 10". Funciona no solo con caracteres ASCII, también con caracteres Unicode:

```Rust
fn main() {
    let cadena = "¡Hola Mundo!";
    println!("Longitud: {}", cadena.len());
}
```

Eso imprimirá "Longitud: 12" porque los caracteres Unicode como "¡" también cuentan.

## Profundizando

El método `.len()` no devuelve el número de caracteres, sino la cantidad de bytes que la cadena ocupa en la memoria. Dado que Rust soporta Unicode, un solo carácter puede ocupar más de un byte. Esta es la razón por la cual `cadena.len()` puede resultar en un número más alto del esperado en algunos casos.

Existen métodos alternativos para contar caracteres en una cadena en lugar de bytes, tales como `cadena.chars().count()`, pero generan más sobrecarga.

Rust implementa este método de manera muy eficiente. A diferencia de otros lenguajes que necesitan recorrer toda la cadena para encontrar su longitud, Rust almacena el tamaño de una cadena como un valor de 8 bytes, lo que permite obtenerlo en tiempo constante, es decir, O(1).

## Ver también

Para ampliar información sobre cadenas en Rust, consulta los siguientes enlaces:
- Documentación oficial de Rust sobre cadenas: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Guía de Rust para el manejo de cadenas: [https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-issues.html](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-issues.html)