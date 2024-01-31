---
title:                "Redondeo de números"
date:                  2024-01-26T03:46:43.869230-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/rounding-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Redondear números significa ajustarlos al número entero más cercano o a una fracción con cierta precisión. Los programadores redondean números para simplificar valores para la legibilidad humana, cumplir con los requisitos de especificaciones o reducir la sobrecarga computacional en operaciones de punto flotante.

## Cómo:
Rust hace que redondear sea muy fácil. Mira estos métodos para los tipos `f32` o `f64`:

```rust
fn main() {
    let num = 2.34567;

    // Redondear al número entero más cercano
    let round = num.round();
    println!("Redondear: {}", round); // Redondear: 2

    // Piso - el mayor entero menor o igual al número
    let floor = num.floor();
    println!("Piso: {}", floor); // Piso: 2

    // Techo - el menor entero mayor o igual al número
    let ceil = num.ceil();
    println!("Techo: {}", ceil); // Techo: 3

    // Truncar - parte entera sin dígitos fraccionales
    let trunc = num.trunc();
    println!("Truncar: {}", trunc); // Truncar: 2

    // Al múltiplo más cercano de una potencia de diez
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Redondeado a 2 decimales: {}", multiple_of_ten); // Redondeado a 2 decimales: 2.35
}
```

## Inmersión Profunda
Históricamente, el redondeo ha sido crucial para ajustar decimales infinitos o números irracionales en espacios digitales limitados—una necesidad para los antiguos ordenadores con escasa memoria. Piensa en un ábaco, pero menos artístico y más matemático.

Alternativas a los métodos nativos de Rust incluyen:
1. Macro `format!` para el formateo de cadenas que redondea por defecto.
2. Crates externos para tareas matemáticas especializadas, como el crate `round` con control más granular.

Bajo el capó, las operaciones de redondeo de Rust cumplen con los estándares IEEE—jerga técnica para "redondea como quiere tu profesor de matemáticas". Además, debido a las representaciones binarias, algunos números no se pueden redondear tradicionalmente, como el 0.1, debido a su representación infinita en binario.

## Ver También
- Documentación de Rust sobre métodos de tipo primitivo: https://doc.rust-lang.org/std/primitive.f64.html
- Estándar IEEE para Aritmética de Punto Flotante (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Crate "round" para redondeo más complejo: https://crates.io/crates/round
