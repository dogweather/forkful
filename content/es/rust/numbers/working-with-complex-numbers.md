---
date: 2024-01-26 04:45:11.371647-07:00
description: "Los n\xFAmeros complejos tienen una parte real y una parte imaginaria,\
  \ y son cruciales en varios campos como la ingenier\xEDa, f\xEDsica y gr\xE1ficos\
  \ por\u2026"
lastmod: '2024-02-25T18:49:55.334600-07:00'
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos tienen una parte real y una parte imaginaria, y\
  \ son cruciales en varios campos como la ingenier\xEDa, f\xEDsica y gr\xE1ficos\
  \ por\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Los números complejos tienen una parte real y una parte imaginaria, y son cruciales en varios campos como la ingeniería, física y gráficos por computadora. Los programadores los usan para resolver ecuaciones que los números reales ordinarios no pueden manejar.

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
