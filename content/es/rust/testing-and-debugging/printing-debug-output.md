---
date: 2024-01-20 17:53:38.103179-07:00
description: "How to: (C\xF3mo hacerlo:) Hist\xF3ricamente, imprimir debug ha sido\
  \ una herramienta elemental en la programaci\xF3n. En Rust, se utiliza el trait\u2026"
lastmod: '2024-04-05T22:51:12.613094-06:00'
model: gpt-4-1106-preview
summary: "(C\xF3mo hacerlo:) Hist\xF3ricamente, imprimir debug ha sido una herramienta\
  \ elemental en la programaci\xF3n."
title: "Imprimiendo salida de depuraci\xF3n"
weight: 33
---

## How to: (Cómo hacerlo:)
```Rust
fn main() {
    let variable_debug = "Rust es genial!";
    println!("Debug: {:?}", variable_debug);
}
```
Salida:
```
Debug: "Rust es genial!"
```

Para estructuras de datos más complejas, derivamos `Debug`:
```Rust
#[derive(Debug)]
struct Estructura {
    campo: i32,
}

fn main() {
    let mi_estructura = Estructura { campo: 42 };
    println!("Debug de estructura: {:?}", mi_estructura);
}
```
Salida:
```
Debug de estructura: Estructura { campo: 42 }
```
Usa `{:#?}` para un formato más bonito:
```Rust
println!("Debug bonito: {:#?}", mi_estructura);
```
Salida:
```
Debug bonito: Estructura {
    campo: 42,
}
```

## Deep Dive (Inmersión Profunda)
Históricamente, imprimir debug ha sido una herramienta elemental en la programación. En Rust, se utiliza el trait `std::fmt::Debug` para añadir funcionalidad de impresión a tipos de datos personalizados. No todos los tipos son imprimibles por defecto; los tipos básicos ya implementan `Debug`, pero para estructuras personalizadas, necesitas derivarlo explícitamente.

Hay alternativas como `log` para outputs más controlados y especializados, y macros como `debug!` que puedes habilitar o deshabilitar en producción. Implementar el trait `Display` personalizado proporciona control total sobre la salida, pero no está específicamente diseñado para depurar.

Rust también introduce una forma bonita e indolora de inspeccionar variables con la macro `dbg!`. Te muestra el valor y la ubicación del código al mismo tiempo:
```Rust
let x = dbg!(42 * 2);
```
Salida:
```
[src/main.rs:1] 42 * 2 = 84
```

## See Also (Consulta También)
- [La documentación oficial de Rust sobre `std::fmt`](https://doc.rust-lang.org/std/fmt/)
- [El libro de Rust sobre depuración](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)
- [Guía sobre cómo escribir logs en Rust](https://rust-lang-nursery.github.io/rust-cookbook/development_tools/debugging/config_log.html)
