---
date: 2024-01-26 04:45:11.371647-07:00
description: "C\xF3mo hacerlo: Rust no tiene soporte incorporado para n\xFAmeros complejos,\
  \ pero crates como `num-complex` te respaldan. As\xED es como lo usas."
lastmod: '2024-03-13T22:44:58.840491-06:00'
model: gpt-4-0125-preview
summary: "Rust no tiene soporte incorporado para n\xFAmeros complejos, pero crates\
  \ como `num-complex` te respaldan."
title: "Trabajando con n\xFAmeros complejos"
weight: 14
---

## Cómo hacerlo:
Rust no tiene soporte incorporado para números complejos, pero crates como `num-complex` te respaldan. Así es como lo usas:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let suma = a + b;
    let producto = a * b;

    println!("Suma: {}", suma); // Suma: 3 - 1i
    println!("Producto: {}", producto); // Producto: 14 - 5i
}
```
Necesitarás agregar `num_complex` a tu `Cargo.toml` para hacer posible esta magia.

## Profundización
Los números complejos fueron concebidos en el siglo XVI pero realmente despegaron en el siglo XVIII cuando matemáticos como Euler comenzaron a jugar con ellos.

Sin operaciones nativas de números complejos, lenguajes como Rust dependen de bibliotecas de terceros. `num-complex` es uno de estos crates y es parte de la colección de crates `num` que tiene como objetivo proporcionar tipos y traits numéricos para Rust.

Vale la pena mencionar que algunos lenguajes (como Python) tienen soporte integrado para números complejos, mientras que otros (como C++, con el encabezado `<complex>`) los proporcionan como parte de la biblioteca estándar. En Rust, la decisión de mantener la biblioteca estándar pequeña significa que a menudo recurrirás a crates creados por la comunidad para funcionalidades adicionales.

## Ver también
- [Libro de Rust](https://doc.rust-lang.org/book/): Para aprender más sobre Rust y cómo trabajar con crates externos.
- [Número Complejo Wikipedia](https://es.wikipedia.org/wiki/N%C3%BAmero_complejo): Para una comprensión más profunda de los números complejos.
