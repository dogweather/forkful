---
date: 2024-01-20 17:53:38.103179-07:00
description: "Imprimir mensajes de depuraci\xF3n nos permite echar un vistazo a lo\
  \ que est\xE1 pasando en nuestro c\xF3digo en tiempo real. Es esencial para entender\
  \ bugs y\u2026"
lastmod: '2024-03-13T22:44:58.849202-06:00'
model: gpt-4-1106-preview
summary: "Imprimir mensajes de depuraci\xF3n nos permite echar un vistazo a lo que\
  \ est\xE1 pasando en nuestro c\xF3digo en tiempo real."
title: "Imprimiendo salida de depuraci\xF3n"
weight: 33
---

## What & Why? (Qué y Por Qué?)
Imprimir mensajes de depuración nos permite echar un vistazo a lo que está pasando en nuestro código en tiempo real. Es esencial para entender bugs y verificar que todo fluye como esperamos.

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
