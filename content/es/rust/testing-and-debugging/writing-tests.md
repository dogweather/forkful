---
title:                "Escribiendo pruebas"
aliases:
- /es/rust/writing-tests.md
date:                  2024-02-03T19:31:55.156988-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas en Rust implica crear comprobaciones automatizadas para asegurar que tu código funciona como se espera. Los programadores hacen esto para detectar errores temprano, facilitar la refactorización y mantener la calidad del código a lo largo del tiempo.

## Cómo hacerlo:

El marco de prueba integrado de Rust soporta pruebas de unidad, integración y documentación sin necesidad de bibliotecas externas. Las pruebas se anotan con `#[test]`, y cualquier función anotada de tal manera se compila como una prueba.

### Escribiendo una Prueba de Unidad:

Coloca las pruebas de unidad en el módulo que están probando usando un submódulo `tests` marcado con `#[cfg(test)]` para asegurar que solo se compilen al probar.

```rust
// lib.rs o main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Ejecutando pruebas:
```shell
$ cargo test
```

Salida:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (or src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Escribiendo Pruebas de Integración:

Las pruebas de integración van en un directorio de pruebas en el nivel superior de tu proyecto, junto a `src`. Cada archivo `.rs` en `tests` se compila como su propia caja separada.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Pruebas con Bibliotecas de Terceros Populares:

Para capacidades de prueba más extensas, la biblioteca `proptest` puede generar una amplia gama de entradas para probar funciones.

Añade `proptest` como una dependencia de desarrollo en `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

Usa `proptest` para ejecutar la misma prueba con muchas entradas generadas automáticamente:

```rust
// dentro de tests/integration_test.rs o un módulo #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

Esto verifica que `add` no entre en pánico para una amplia gama de entradas `i32`.
